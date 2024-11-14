import json
import time
import typing as t
from datetime import datetime
from io import BytesIO
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)
from hrflow_connectors.core.warehouse import ReadMode
from hrflow_connectors.v1.connectors.bullhorn.schemas import (
    BullhornJob,
    BullhornProfile,
)
from hrflow_connectors.v1.connectors.bullhorn.utils.authentication import auth


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


class ReadParameters(BaseParameters):
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

    count: int = Field(
        ...,
        description="Number of items to be returned",
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
                "Failed to push profile from to Bullhorn status_code={} response={}"
                .format(response.status_code, response.text)
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


def authenticate(parameters):
    return auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )


def make_request(method, url, params, adapter, json=None):
    response = method(url, params=params, data=json)
    if response.status_code == 401:
        adapter.info("Auth token expired, regenerating...")
        auth_info = authenticate()
        params["BhRestToken"] = auth_info["BhRestToken"]
        response = method(url, params=params, data=json)
    return response


def handle_response(response, adapter):
    if not response.ok:
        adapter.error(
            f"Request failed, status_code={response.status_code},"
            f" response={response.text}"
        )
        return None
    return response.json()


def search_entity(entity, rest_url, bh_rest_token, query, fields, adapter):
    search_url = f"{rest_url}search/{entity}"
    params = {
        "BhRestToken": bh_rest_token,
        "query": query,
        "fields": fields,
        "sort": "id",
    }
    response = make_request(requests.get, search_url, params, adapter)
    return handle_response(response, adapter)


def create_or_update_entity(entity, rest_url, params, data, adapter, entity_id=None):
    url = f"{rest_url}entity/{entity}"
    method = requests.post if entity_id else requests.put
    if entity_id:
        url = f"{url}/{entity_id}"
    response = make_request(method, url, params, adapter, json.dumps(data))
    return handle_response(response, adapter)


def check_entity_files(entity, rest_url, params, entity_id, adapter):
    url = f"{rest_url}entityFiles/{entity}/{entity_id}"
    response = requests.get(url, params=params)
    return handle_response(response, adapter)


def write_application(
    adapter: LoggerAdapter,
    parameters: WriteApplicationsParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    auth_info = authenticate(parameters)
    rest_url = auth_info["restUrl"]
    bh_rest_token = auth_info["BhRestToken"]
    params = {"BhRestToken": bh_rest_token}
    adapter.info(f"connexion info {params}, rest_url: {rest_url}")

    for profile in profiles:
        attachment = profile.pop("attachment")
        profile["source"] = parameters.source or profile.get("source")
        profile["status"] = parameters.status_when_created or profile.get("status")
        email = profile["email"]
        adapter.info(f"checking if candidate with {email} already exists")
        search_results = search_entity(
            "Candidate",
            rest_url,
            bh_rest_token,
            f"(email:{email} OR email2:{email}) AND isDeleted:0",
            (
                "id,isDeleted,dateAdded,status,source,email,"
                "firstName,lastName,name,mobile,address"
            ),
            adapter,
        )

        if not search_results:
            failed_profiles.append(profile)
            continue
        adapter.info(f"search profile response {search_results}")
        candidate_exists = search_results["count"] > 0
        candidate_data = search_results["data"][0] if candidate_exists else {}
        candidate_id = candidate_data.get("id") if candidate_exists else None

        if candidate_exists:
            profile.update(
                {
                    "firstName": candidate_data.get("firstName") or profile.get(
                        "firstName"
                    ),
                    "lastName": candidate_data.get("lastName") or profile.get(
                        "lastName"
                    ),
                    "name": candidate_data.get("name") or profile.get("name"),
                    "address": candidate_data.get("address") or profile.get("address"),
                    "mobile": candidate_data.get("mobile") or profile.get("mobile"),
                    "status": candidate_data.get("status") or profile.get("status"),
                }
            )
        adapter.info("creating or updating the candidate")
        candidate_response = create_or_update_entity(
            "Candidate", rest_url, params, profile, adapter, candidate_id
        )
        if not candidate_response:
            failed_profiles.append(profile)
            continue
        adapter.info(f"candidate creation response {candidate_response}")
        if not candidate_exists:
            candidate_id = candidate_response.get("changedEntityId")

        attachment_exists = False
        if candidate_exists and attachment:
            entity_files = check_entity_files(
                "Candidate", rest_url, params, candidate_id, adapter
            )
            if entity_files:
                attachments = entity_files.get("EntityFiles", [])
                if attachments and attachment["name"] == attachments[0]["name"]:
                    attachment_exists = True
        adapter.info(f"attachment for the candidate exists {attachment_exists}")
        if not attachment_exists and attachment:
            attachment_response = make_request(
                requests.put,
                f"{rest_url}file/Candidate/{candidate_id}",
                params,
                adapter,
                json.dumps(attachment),
            )
            if not handle_response(attachment_response, adapter):
                failed_profiles.append(profile)
                continue
            attachment_response = handle_response(attachment_response, adapter)
            adapter.info(f"attachment response {attachment_response}")
        adapter.info(
            "Verifying if candidate had already applied for the job"
            f" {parameters.job_id}"
        )
        job_submission_results = search_entity(
            "JobSubmission",
            rest_url,
            bh_rest_token,
            f"candidate.id:{candidate_id} AND jobOrder.id:{parameters.job_id}",
            "id,status,dateAdded",
            adapter,
        )

        if not job_submission_results:
            failed_profiles.append(profile)
            continue
        adapter.info(f"search job_submission response {job_submission_results}")
        job_submission_exists = job_submission_results["count"] > 0
        job_submission_id = (
            job_submission_results["data"][0]["id"] if job_submission_exists else None
        )

        job_submission_payload = {
            "candidate": {"id": candidate_id},
            "jobOrder": {"id": parameters.job_id},
            "status": parameters.status_when_created,
            "dateWebResponse": int(time.time() * 1000),
        }
        adapter.info("Creating or updating if candidate jobSubmission")
        job_submission_response = create_or_update_entity(
            "JobSubmission",
            rest_url,
            params,
            job_submission_payload,
            adapter,
            job_submission_id,
        )
        if not job_submission_response:
            failed_profiles.append(profile)
        adapter.info(f"creation of job_submission response {job_submission_response}")
    return failed_profiles


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    start = 0
    auth_retries = 0
    total_returned = 0
    should_break = False
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
                f"{parameters.query} AND "
                f"dateLastModified:[{last_modified_date_filter} TO *]"
            )
            jobs_url = f"{authentication['restUrl']}search/JobOrder"
            params = {
                "query": query,
                "fields": parameters.fields,
                "sort": "dateLastModified,id",
                "start": str(start),
            }

            if parameters.count:
                params["count"] = parameters.count

            headers = {"BhRestToken": authentication["BhRestToken"]}

            response = requests.get(url=jobs_url, params=params, headers=headers)

            response = response.json()
            start = response["start"] + response["count"]
            data = response["data"]
            for job in data:
                if parameters.count and total_returned >= parameters.count:
                    should_break = True
                    break
                if (
                    read_mode is ReadMode.incremental
                    and job["dateLastModified"] == last_modified_date
                    and job["id"] <= last_id
                ):
                    adapter.info("job with id <= last_id")
                    continue
                yield job
                total_returned += 1
            if should_break:
                break
            if start >= response["total"]:
                break

        except requests.HTTPError as e:
            if e.response.status_code == 401:
                adapter.info(
                    "Received 401 error. Retrying authentication to continue fetching"
                    " jobs."
                )
                if auth_retries > 2:
                    raise Exception(
                        f" retries the authentication {auth_retries}"
                        " will stop the execution"
                    )

                authentication = auth(
                    parameters.username,
                    parameters.password,
                    parameters.client_id,
                    parameters.client_secret,
                    refresh_token=authentication["refresh_token"],
                )
                auth_retries += 1
                continue
            else:
                adapter.error("Failed to fetch jobs from Bullhorn.")
                raise e


def read_profiles_parsing(
    adapter: LoggerAdapter,
    parameters: ReadParameters,
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
    auth_retries = 0
    total_returned = 0
    should_break = False
    last_id = None

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
            "Error while applying a transformation date on last modified date to"
            " perform filtering"
        )

    while True:
        try:
            query = (
                f"{parameters.query} AND "
                f"dateLastModified:[{last_modified_date_filter} TO *]"
            )

            profiles_url = f"{authentication['restUrl']}search/Candidate"
            params = {
                "query": query,
                "fields": parameters.fields,
                "sort": "dateLastModified,id",
                "start": str(start),
            }

            if parameters.count:
                params["count"] = parameters.count

            headers = {"BhRestToken": authentication["BhRestToken"]}

            response = requests.get(url=profiles_url, params=params, headers=headers)
            if response.status_code // 100 != 2:
                adapter.error(
                    "Failed to pull profiles from Bullhorn"
                    f" status_code={response.status_code} response={response.text}"
                )
                raise Exception("Failed to pull profiles from Bullhorn")
            response = response.json()

            start = response["start"] + response["count"]
            data = response["data"]
            total = response["total"]

            for profile in data:
                if parameters.count and total_returned >= parameters.count:
                    should_break = True
                    break
                if (
                    read_mode is ReadMode.incremental
                    and profile["dateLastModified"] == last_modified_date
                    and profile["id"] <= last_id
                ):
                    adapter.info("Profile with id <= last_id")
                    continue

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
                    profile_file = BytesIO(file)
                    profile["cvFile"] = profile_file

                total_returned += 1
                yield profile

            if should_break:
                break

            if start >= total:
                break

        except requests.HTTPError as e:
            if e.response.status_code == 401:
                adapter.info(
                    "Received 401 error. Retrying authentication to continue fetching"
                    " profiles."
                )
                if auth_retries > 2:
                    raise Exception(
                        f"Retries exceeded for authentication ({auth_retries})."
                        " Stopping execution."
                    )

                authentication = auth(
                    parameters.username,
                    parameters.password,
                    parameters.client_id,
                    parameters.client_secret,
                    refresh_token=authentication["refresh_token"],
                )
                auth_retries += 1
                continue
            else:
                adapter.error("Failed to fetch profiles from Bullhorn.")
                raise e


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
                "Failed to pull profiles from Bullhorn status_code={} response={}"
                .format(response.status_code, response.text)
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


def transform_timestamp(timestamp: t.Optional[t.Union[float, int]]) -> t.Optional[str]:
    if timestamp is None:
        return None
    # Convert the Unix timestamp (in milliseconds) to a datetime object
    dt = datetime.utcfromtimestamp(int(timestamp) / 1000)
    # Format the datetime object to something like 20221215121030
    transformed_date = dt.strftime("%Y%m%d%H%M%S")
    return transformed_date


def item_to_read_from(item: t.Dict) -> str:
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
        parameters=ReadParameters,
        function=read_profiles_parsing,
        endpoints=[],
        supports_incremental=True,
        item_to_read_from=item_to_read_from,
    ),
)

BullhornJobWarehouse = Warehouse(
    name="Bullhorn Jobs",
    data_schema=BullhornJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadParameters,
        function=read_jobs,
        supports_incremental=True,
        item_to_read_from=item_to_read_from,
    ),
)
