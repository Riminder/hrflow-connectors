import json
import time
import typing as t
from datetime import datetime
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.bullhorn.schemas import BullhornJob, BullhornProfile
from hrflow_connectors.connectors.bullhorn.utils.authentication import auth
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)
from hrflow_connectors.core.warehouse import ReadMode


class BaseParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="Client identifier for Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        description="Client secret identifier for Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )
    password: str = Field(
        ...,
        description="Password for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )
    username: str = Field(
        ...,
        description="Username for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )


class WriteProfilesParameters(BaseParameters):
    pass


class WriteApplicationsParameters(BaseParameters):
    job_id: str = Field(
        ...,
        description="id for the job in Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    status_when_created: str = Field(
        ...,
        description="The status of the application when created in Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    source: str = Field(
        None,
        description="The source of the application to be created in Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadJobsParameters(BaseParameters):
    last_modified_date: str = Field(
        ...,
        description="Last Modified Date in timestamp",
        repr=False,
        field_type=FieldType.Auth,
    )

    fields: str = Field(
        ...,
        description="Fields to be retrieved from Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    query: str = Field(
        ...,
        description="the query parameters",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadProfileParameters(BaseParameters):
    pass


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} profiles".format(len(profiles)))
    failed_profiles = []
    authentication = auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )

    for profile in profiles:
        # Split profile in four parts: Body, Education, Experience and Attachements
        profile_body_dict = profile
        create_profile_body = profile_body_dict["create_profile_body"]
        enrich_profile_education = profile_body_dict["enrich_profile_education"]
        enrich_profile_experience = profile_body_dict["enrich_profile_experience"]
        enrich_profile_attachment = profile_body_dict["enrich_profile_attachment"]

        rest_url = authentication["restUrl"]
        params = {"BhRestToken": authentication["BhRestToken"]}

        candidate_url = rest_url + "entity/Candidate"
        response = requests.put(
            url=candidate_url, json=create_profile_body, params=params
        )

        # Unable to push profile
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile from to Bullhorn"
                " status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            failed_profiles.append(profile)
            continue

        candidate_id = response.json()
        candidate_id = str(candidate_id["changedEntityId"])

        # Enrich profile education
        for education in enrich_profile_education:
            education["candidate"]["id"] = candidate_id
            education_url = rest_url + "entity/CandidateEducation"
            response = requests.put(url=education_url, json=education, params=params)

        # Enrich profile experience
        for experience in enrich_profile_experience:
            experience["candidate"]["id"] = candidate_id
            experience_url = rest_url + "entity/CandidateWorkHistory"
            response = requests.put(url=experience_url, json=experience, params=params)

        # Enrich profile attachements
        for attachment in enrich_profile_attachment:
            attachment_url = rest_url + "file/Candidate/" + str(candidate_id)
            response = requests.put(url=attachment_url, json=attachment, params=params)

    return failed_profiles


def write_application(
    adapter: LoggerAdapter,
    parameters: WriteApplicationsParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info(f"Pushing {len(profiles)} profiles")
    failed_profiles = []

    def authenticate():
        return auth(
            parameters.username,
            parameters.password,
            parameters.client_id,
            parameters.client_secret,
        )

    def make_request(method, url, params, json=None):
        response = method(url, params=params, data=json)
        if response.status_code == 401:
            adapter.info("Auth token expired, regenerating...")
            auth_info = authenticate()
            params["BhRestToken"] = auth_info["BhRestToken"]
            response = method(url, params=params, data=json)
        return response

    auth_info = authenticate()
    rest_url = auth_info["restUrl"]
    bh_rest_token = auth_info["BhRestToken"]
    params = {"BhRestToken": bh_rest_token}
    adapter.info(f"connexion info {params}, rest_url: {rest_url}")

    for profile in profiles:
        attachment = profile.pop("attachment")
        if parameters.source:
            profile["source"] = parameters.source
        if parameters.status_when_created:
            profile["status"] = parameters.status_when_created
        email = profile["email"]
        # Verify if the Candidate exists
        search_url = f"{rest_url}search/Candidate"
        search_query = f"(email:{email} OR email2:{email}) AND isDeleted:0"
        search_response = make_request(
            requests.get,
            search_url,
            params={
                "BhRestToken": bh_rest_token,
                "query": search_query,
                "fields": (
                    "id,isDeleted,dateAdded,status,source,"
                    "email,firstName,lastName,name,mobile,address"
                ),
                "sort": "id",
            },
        )

        if search_response.status_code != 200:
            adapter.error(
                f"Search failed for {email}, status_code={search_response.status_code}"
                f" And full response {search_response.text}"
            )
            failed_profiles.append(profile)
            continue
        adapter.info(f"search response {search_response.text}")
        search_results = search_response.json()
        candidate_exists = search_results["count"] > 0
        if candidate_exists:
            candidate_data = search_results["data"][0]
            candidate_id = candidate_data["id"]

            # Update profile with existing candidate data if available
            profile.update(
                {
                    "firstName": candidate_data.get(
                        "firstName", profile.get("firstName")
                    ),
                    "lastName": candidate_data.get("lastName", profile.get("lastName")),
                    "name": candidate_data.get("name", profile.get("name")),
                    "address": candidate_data.get("address", profile.get("address")),
                    "mobile": candidate_data.get("mobile", profile.get("mobile")),
                    "status": candidate_data.get("status", profile.get("status")),
                }
            )
        else:
            candidate_id = None
        # Create or Update the Candidate
        candidate_url = f"{rest_url}entity/Candidate"
        method = requests.post if candidate_exists else requests.put
        url = f"{candidate_url}/{candidate_id}" if candidate_exists else candidate_url
        profile = json.dumps(profile)
        adapter.info(f"url to push candidate {url}")
        adapter.info(f"params {params}")
        adapter.info(f"profile {profile}")
        profile_response = make_request(method, url, params, json=profile)

        if profile_response.status_code // 100 != 2:
            adapter.error(
                f"Failed to push profile for {email},"
                f" status_code={profile_response.status_code}"
                f" and response={profile_response.text}"
            )
            failed_profiles.append(profile)
            continue

        adapter.info(f"profile_response {profile_response.text}")
        if not candidate_exists:
            candidate_id = profile_response.json().get("changedEntityId")

        # Verify if the Attachment exists already
        attachment_exists = False
        attachment_url = f"{rest_url}file/Candidate/{candidate_id}"

        if candidate_exists:
            get_candidate_attachment_url = (
                f"{rest_url}entityFiles/Candidate/{candidate_id}"
            )
            response = requests.get(get_candidate_attachment_url, params=params)

            if response.status_code == 200:
                attachments = response.json().get("EntityFiles", [])
                if attachments and attachment["name"] == attachments[0]["name"]:
                    attachment_exists = True
        # Post the Attachment
        if not attachment_exists:
            attachment_response = make_request(
                requests.put, attachment_url, params=params, json=json.dumps(attachment)
            )

            if attachment_response.status_code // 100 != 2:
                adapter.info(
                    f"Failed to push attachment for candidate ID {candidate_id},"
                    f" status_code={attachment_response.status_code},"
                    f" response is {attachment_response.text}"
                )
                failed_profiles.append(profile)
            adapter.info(f"attachment_response {attachment_response.text}")

        # Verify the candidtae has already applied for the job
        job_submission_search_url = f"{rest_url}search/JobSubmission"
        job_submission_search_query = (
            f"candidate.id:{candidate_id} AND jobOrder.id:{parameters.job_id}"
        )
        job_submission_search_response = make_request(
            requests.get,
            job_submission_search_url,
            params={
                "BhRestToken": bh_rest_token,
                "query": job_submission_search_query,
                "fields": "id,status,dateAdded",
                "sort": "id",
            },
        )

        if job_submission_search_response.status_code != 200:
            adapter.info(
                f"Failed to search job submissions for candidate ID {candidate_id},"
                f" status_code={job_submission_search_response.status_code},"
                f" response={job_submission_search_response.text}"
            )
            failed_profiles.append(profile)
            continue
        adapter.info(
            f"job_submission_search_response {job_submission_search_response.text}"
        )
        job_submission_search_results = job_submission_search_response.json()
        job_submission_exists = job_submission_search_results["count"] > 0
        job_submission_id = (
            job_submission_search_results["data"][0]["id"]
            if job_submission_exists
            else None
        )
        job_submission_url = f"{rest_url}entity/JobSubmission"
        job_submission_payload = {
            "candidate": {"id": candidate_id},
            "jobOrder": {"id": parameters.job_id},
            "status": parameters.status_when_created,
            "dateWebResponse": int(
                time.time() * 1000
            ),  # Unix timestamp in milliseconds
        }

        # Create or update the application
        method = requests.put if not job_submission_exists else requests.post
        if job_submission_exists:
            job_submission_url = f"{job_submission_url}/{job_submission_id}"

        job_submission_response = make_request(
            method, job_submission_url, params, json=json.dumps(job_submission_payload)
        )

        if job_submission_response.status_code // 100 != 2:
            adapter.info(
                f"Failed to submit job application for candidate ID {candidate_id},"
                f" status_code={job_submission_response.status_code},"
                f" response={job_submission_response.text}"
            )
            failed_profiles.append(profile)
        adapter.info(f"job_submission_response {job_submission_response.text}")

    return failed_profiles


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    start = 0

    authentication = auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )
    if read_mode is ReadMode.sync:
        if parameters.last_modified_date is None:
            raise Exception("last_modified_date cannot be None in ReadMode.sync")
        last_modified_date = parameters.last_modified_date
    else:
        if parameters.last_modified_date is not None:
            adapter.warning(
                "last_modified_date is ignored in ReadMode.incremental, using"
                " read_from instead"
            )
        if read_from:
            try:
                read_from = json.loads(read_from)
                last_modified_date = read_from["last_modified_date"]
                last_id = read_from["last_id"]
            except json.JSONDecodeError as e:
                raise Exception(f"Failed to JSON parse read_from={read_from} error={e}")
            except KeyError as e:
                raise Exception(
                    "Failed to find expected key in"
                    f" read_from={read_from} error={repr(e)}"
                )
        else:
            last_modified_date = parameters.last_modified_date

    last_modified_date_filter = transform_timestamp(last_modified_date)
    if not last_modified_date_filter:
        raise Exception(
            "error while applying a transformation date on last modified date to"
            " perform filtering"
        )

    while True:
        try:
            query = (
                f"{parameters.query} AND"
                f" dateLastModified:[{last_modified_date_filter}%20TO%20*]"
            )
            jobs_url = (
                authentication["restUrl"]
                + f"search/JobOrder?query={query}&fields="
                + parameters.fields
                + "&BhRestToken="
                + authentication["BhRestToken"]
                + "&start="
                + str(start)
            )

            response = requests.get(url=jobs_url)

            response = response.json()
            start = response["start"] + response["count"]
            data = response["data"]

            for job in data:
                if (
                    job["dateLastModified"] == last_modified_date
                    and job["id"] <= last_id
                ):
                    adapter.info("job with id <= last_id")
                    continue
                yield job

            if start >= response["total"]:
                break

        except requests.HTTPError as e:
            if e.response.status_code == 401:
                adapter.info(
                    "Received 401 error. Retrying authentication to continue fetching"
                    " jobs."
                )
                authentication = auth(
                    parameters.username,
                    parameters.password,
                    parameters.client_id,
                    parameters.client_secret,
                    refresh_token=authentication["refresh_token"],
                )
                continue
            else:
                adapter.error("Failed to fetch jobs from Bullhorn.")
                raise e


def read_profiles_parsing(
    adapter: LoggerAdapter,
    parameters: ReadProfileParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    authentication = auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )
    start = 0
    while True:
        profiles_url = (
            authentication["restUrl"] + "myCandidates?fields=*&start=" + str(start)
        )
        headers = {"BhRestToken": authentication["BhRestToken"]}

        response = requests.get(url=profiles_url, headers=headers)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profiles from Bullhorn"
                " status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            raise Exception("Failed to pull profiles from Bullhorn")
        response = response.json()

        start = response["start"] + response["count"]
        data = response["data"]
        total = response["total"]

        for profile in data:
            profile["cvFile"] = None
            url_files = (
                authentication["restUrl"]
                + "entityFiles/Candidate/"
                + str(profile["id"])
            )
            headers = {"BhRestToken": authentication["BhRestToken"]}
            response = requests.get(url=url_files, headers=headers)
            response = response.json()

            last_cv = None
            curr_entity = None
            if len(response["EntityFiles"]) > 0:
                for entity_file in response["EntityFiles"]:
                    if entity_file["type"] == "Resume":
                        if not curr_entity:
                            curr_entity = entity_file
                            last_cv = entity_file["id"]
                        elif curr_entity["dateAdded"] < entity_file["dateAdded"]:
                            curr_entity = entity_file
                            last_cv = entity_file["id"]

            if last_cv is not None:
                url_cv = (
                    authentication["restUrl"]
                    + "/file/Candidate/"
                    + str(profile["id"])
                    + "/"
                    + str(last_cv)
                    + "/raw"
                )
                response = requests.get(url=url_cv, headers=headers)

                file = response.content
                with open("cv_file.pdf", "wb") as f:
                    f.write(file)

                with open("cv_file.pdf", "rb") as f:
                    profile_file = f.read()
                    profile["cvFile"] = profile_file

                yield profile

        if start >= total:
            break


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfileParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    authentication = auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )
    fields = """firstName,lastName,name,email,mobile,dateOfBirth,gender,address,dateAvailable,status,employeeType,activePlacements,
    skillSet,id,educations,workHistories"""
    start = 0
    while True:
        profiles_url = (
            authentication["restUrl"]
            + "myCandidates?fields="
            + fields
            + "&start="
            + str(start)
        )
        headers = {"BhRestToken": authentication["BhRestToken"]}

        response = requests.get(url=profiles_url, headers=headers)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profiles from Bullhorn"
                " status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            raise Exception("Failed to pull profiles from Bullhorn")
        response = response.json()

        start = response["start"] + response["count"]
        total = response["total"]
        data = response["data"]

        for profile in data:
            # Enrich profile education
            education_ids = []
            educations = []
            for ed in profile["educations"]["data"]:
                education_ids.append(ed["id"])
            for id in education_ids:
                education_url = (
                    authentication["restUrl"]
                    + "entity/CandidateEducation/"
                    + str(id)
                    + "?fields=*"
                )
                response = requests.get(url=education_url, headers=headers)
                response = response.json()
                educations.append(response["data"])

            # Enrich profile work history
            work_history_ids = []
            work_histories = []
            for work_history in profile["workHistories"]["data"]:
                work_history_ids.append(work_history["id"])
            for id in work_history_ids:
                work_history_url = (
                    authentication["restUrl"]
                    + "entity/CandidateWorkHistory/"
                    + str(id)
                    + "?fields=*"
                )
                response = requests.get(url=work_history_url, headers=headers)
                response = response.json()
                work_histories.append(response["data"])

            profile["educations"] = educations
            profile["workHistories"] = work_histories

            yield profile

        if start >= total:
            break


def transform_timestamp(timestamp):
    if not timestamp:
        return None
    try:
        # Convert the Unix timestamp (in milliseconds) to a datetime object
        dt = datetime.fromtimestamp(int(timestamp) / 1000)
        # Format the datetime object to something like 20221215121030
        transformed_date = dt.strftime("%Y%m%d%H%M%S")
        return transformed_date
    except Exception:
        return None


def item_to_read_from_job(item: t.Dict) -> str:
    return json.dumps(
        dict(last_modified_date=item["dateLastModified"], last_id=item["id"])
    )


BullhornProfileWarehouse = Warehouse(
    name="Bullhorn Profiles",
    data_schema=BullhornProfile,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters, function=write, endpoints=[]
    ),
    read=WarehouseReadAction(
        parameters=ReadProfileParameters, function=read_profiles, endpoints=[]
    ),
)

BullhornApplicationWarehouse = Warehouse(
    name="Bullhorn Applications",
    data_schema=BullhornProfile,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteApplicationsParameters, function=write_application, endpoints=[]
    ),
)

BullhornProfileParsingWarehouse = Warehouse(
    name="Bullhorn Profiles",
    data_schema=BullhornProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfileParameters, function=read_profiles_parsing, endpoints=[]
    ),
)

BullhornJobWarehouse = Warehouse(
    name="Bullhorn Jobs",
    data_schema=BullhornJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        endpoints=[],
    ),
    supports_incremental=True,
    item_to_read_from=item_to_read_from_job,
)
