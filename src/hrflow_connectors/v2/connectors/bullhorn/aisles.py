import json
import time
import typing as t
from datetime import datetime
from io import BytesIO
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.bullhorn.schemas import (
    BullhornJob,
    BullhornProfile,
)
from hrflow_connectors.v2.connectors.bullhorn.utils.authentication import auth
from hrflow_connectors.v2.core.common import Entity, Mode
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)


class AuthParameters(Struct):
    client_id: Annotated[
        str,
        Meta(
            description="Client identifier for Bullhorn",
        ),
    ]
    client_secret: Annotated[
        str,
        Meta(
            description="Client secret identifier for Bullhorn",
        ),
    ]
    password: Annotated[
        str,
        Meta(
            description="Password for Bullhorn login",
        ),
    ]
    username: Annotated[
        str,
        Meta(
            description="Username for Bullhorn login",
        ),
    ]


class UpdateApplicationsCriterias(Struct):
    job_id: Annotated[
        str,
        Meta(
            description="id for the job in Bullhorn",
        ),
    ]
    # maybe should be optional
    status_when_created: Annotated[
        str,
        Meta(
            description="The status of the application when created in Bullhorn",
        ),
    ]
    # maybe should not be a parameters
    source: Annotated[
        str,
        Meta(
            description="The source of the application to be created in Bullhorn",
        ),
    ]


class BaseParameters(Struct):
    limit: Annotated[
        t.Optional[int],
        Meta(
            description="Number of items to pull, ignored if not provided.",
        ),
    ] = None


class BaseJobsParameters(BaseParameters, kw_only=True):
    fields: Annotated[
        str,
        Meta(
            min_length=2,
            description="List of job fields to be retrieved from Bullhorn",
        ),
    ] = (
        "address,assignedUsers,businessSectors,categories,clientBillRate,"
        "clientContact,clientCorporation,costCenter,customInt1,customInt2,"
        "customText1,customText10,customText11,customText12,customText13,"
        "customText2,customText3,customText4,customText5,customText6,"
        "customText7,customText8,customText9,customTextBlock1,customTextBlock2,"
        "customTextBlock3,customTextBlock4,customTextBlock5,dateAdded,dateEnd,"
        "degreeList,description,durationWeeks,educationDegree,employmentType,"
        "feeArrangement,hoursOfOperation,hoursPerWeek,isOpen,isWorkFromHome,"
        "markUpPercentage,numOpenings,onSite,payRate,salary,salaryUnit,skills,"
        "skillList,source,specialties,startDate,status,title,type,willRelocate,"
        "owner"
    )
    query: Annotated[
        str,
        Meta(
            description=(
                "This query will restrict the results retrieved from Bullhorn based on"
                " the specified conditions"
            ),
        ),
    ] = "isDeleted:0 AND isOpen:true"


class ReadCreatedJobsCriterias(BaseJobsParameters, kw_only=True):
    created_date: Annotated[
        datetime,
        Meta(
            description="The creation date from which you want to pull jobs",
        ),
    ]


class ReadUpdatedJobsCriterias(BaseJobsParameters, kw_only=True):
    last_modified_date: Annotated[
        datetime,
        Meta(
            description="The modification date from which you want to pull jobs",
        ),
    ]


class ReadArchivedJobsCriterias(BaseParameters, kw_only=True):
    last_modified_date: Annotated[
        datetime,
        Meta(
            description=(
                "The modification date from which you want to pull jobs and archive"
                " them"
            ),
        ),
    ]
    query: Annotated[
        str,
        Meta(
            description=(
                "This query will restrict the results retrieved from Bullhorn based on"
                " the specified conditions"
            ),
        ),
    ] = "isDeleted:0 AND isOpen:true"
    fields: Annotated[
        str,
        Meta(
            description="Field to be used as reference for archiving",
        ),
    ] = "id"


class BaseProfilesParameters(BaseParameters):
    fields: Annotated[
        str,
        Meta(
            min_length=2,
            description="List of profile fields to be retrieved from Bullhorn",
        ),
    ] = (
        "address,businessSectors,categories,companyName,customInt4,customInt5,"
        "customInt6,customText1,customText10,customText11,customText12,"
        "customText13,customText14,customText15,customText16,customText18,"
        "customText23,customText24,customText25,customText4,customText5,"
        "customText6,customText9,dateAdded,dateAvailable,dateAvailableEnd,"
        "dateLastModified,dateOfBirth,dayRate,dayRateLow,degreeList,"
        "desiredLocations,description,disability,educations,email,email2,"
        "employmentPreference,ethnicity,experience,firstName,id,lastName,"
        "mobile,name,namePrefix,occupation,owner,phone,primarySkills,"
        "secondaryOwners,secondarySkills,salary,salaryLow,skillSet,"
        "source,specialties,status,userDateAdded,veteran,willRelocate,"
        "workHistories,workPhone"
    )
    query: Annotated[
        str,
        Meta(
            description=(
                "This query will restrict the results retrieved from Bullhorn based on"
                " the specified conditions"
            ),
        ),
    ] = "isDeleted:0"


class ReadCreatedProfilesCriterias(BaseProfilesParameters, kw_only=True):
    created_date: Annotated[
        datetime,
        Meta(
            description="The creation date from which you want to pull profiles",
        ),
    ]
    parse_resume: Annotated[
        bool,
        Meta(
            description=(
                "If True, resumes will be retrieved and parsed along with the profile"
                " data"
            ),
        ),
    ] = False


class ReadUpdatedProfilesCriterias(BaseProfilesParameters, kw_only=True):
    last_modified_date: Annotated[
        datetime,
        Meta(
            description="The modification date from which you want to pull profiles",
        ),
    ]
    parse_resume: Annotated[
        bool,
        Meta(
            description=(
                "If True, resumes will be retrieved and parsed along with the profile"
                " data"
            ),
        ),
    ] = False


class ReadArchivedProfilesCriterias(BaseParameters, kw_only=True):
    last_modified_date: Annotated[
        datetime,
        Meta(
            description="The modification date from which you want to pull profiles",
        ),
    ]
    query: Annotated[
        str,
        Meta(
            description=(
                "This query will restrict the results retrieved from Bullhorn based on"
                " the specified conditions"
            ),
        ),
    ] = "isDeleted:0"
    fields: Annotated[
        str,
        Meta(
            description="Field to be used as reference for archiving",
        ),
    ] = "id"


def make_request(
    method, url, params, auth_parameters: AuthParameters, adapter, json=None
):
    response = method(url, params=params, data=json)
    if response.status_code == 401:
        adapter.info("Auth token expired, regenerating...")
        auth_info = auth(
            auth_parameters.username,
            auth_parameters.password,
            auth_parameters.client_id,
            auth_parameters.client_secret,
        )
        params["BhRestToken"] = auth_info["BhRestToken"]
        response = method(url, params=params, data=json)
    return handle_response(response, adapter)


def handle_response(response, adapter):
    if not response.ok:
        adapter.error(
            f"Request failed with status_code={response.status_code},"
            f" response={response.text}"
        )
        return None
    return response.json()


def search_entity(
    entity, rest_url, bh_rest_token, query, fields, adapter, auth_parameters
):
    search_url = f"{rest_url}search/{entity}"
    params = {
        "BhRestToken": bh_rest_token,
        "query": query,
        "fields": fields,
        "sort": "id",
    }
    response = make_request(requests.get, search_url, params, auth_parameters, adapter)
    return response


def create_entity(entity, rest_url, params, data, auth_parameters, adapter):
    url = f"{rest_url}entity/{entity}"
    response = make_request(
        requests.post, url, params, auth_parameters, adapter, json.dumps(data)
    )
    return response


def update_entity(entity, entity_id, rest_url, params, data, auth_parameters, adapter):
    url = f"{rest_url}entity/{entity}/{entity_id}"
    response = make_request(
        requests.put, url, params, auth_parameters, adapter, json.dumps(data)
    )
    return response


def check_entity_files(entity, rest_url, params, entity_id, auth_parameters, adapter):
    url = f"{rest_url}entityFiles/{entity}/{entity_id}"
    response = make_request(requests.get, url, params, auth_parameters, adapter)
    return response


def upload_attachment(
    entity, entity_id, rest_url, params, attachment, adapter, auth_parameters
):
    url = f"{rest_url}file/{entity}/{entity_id}"
    attachment_response = make_request(
        requests.put, url, params, auth_parameters, adapter, json.dumps(attachment)
    )
    return attachment_response


def update_application(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateApplicationsCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    failed_profiles = []
    auth_info = auth(
        auth_parameters.username,
        auth_parameters.password,
        auth_parameters.client_id,
        auth_parameters.client_secret,
    )
    rest_url = auth_info["restUrl"]
    bh_rest_token = auth_info["BhRestToken"]
    params = {"BhRestToken": bh_rest_token}

    for profile in items:
        attachment = profile.pop("attachment", None)
        profile["source"] = [parameters.source or profile.get("source")]
        profile["status"] = parameters.status_when_created or profile.get("status")
        email = profile["email"]

        adapter.info(f"Checking if candidate with email: {email} already exists.")
        search_results = search_entity(
            "Candidate",
            rest_url,
            bh_rest_token,
            f"(email:{email} OR email2:{email}) AND isDeleted:0",
            (
                "id,isDeleted,dateAdded,status,source,email,firstName,"
                "lastName,name,mobile,address"
            ),
            adapter,
            parameters,
        )

        if not search_results:
            failed_profiles.append(profile)
            continue

        if search_results["count"] == 0:
            adapter.info(f"Creating candidate with email: {email}")
            candidate_response = create_entity(
                "Candidate", rest_url, params, profile, parameters, adapter
            )
            if not candidate_response:
                failed_profiles.append(profile)
                continue
            candidate_id = candidate_response["changedEntityId"]
            attachment_exists = False

        else:
            candidate_data = search_results["data"][0]
            candidate_id = candidate_data.get("id")

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
                    "source": candidate_data.get("source") or profile.get("source"),
                }
            )

            adapter.info(f"Updating candidate with ID: {candidate_id}")
            candidate_response = update_entity(
                "Candidate",
                candidate_id,
                rest_url,
                params,
                profile,
                parameters,
                adapter,
            )

            if not candidate_response:
                failed_profiles.append(profile)
                continue

            if attachment:
                adapter.info(
                    f"Checking if attachment exists for candidate {candidate_id}"
                )
                entity_files = check_entity_files(
                    "Candidate", rest_url, params, candidate_id, parameters, adapter
                )
                attachment_exists = any(
                    file["name"] == attachment["name"]
                    for file in entity_files.get("EntityFiles", [])
                )

        if not attachment_exists:
            adapter.info("Uploading attachment")
            attachment_response = upload_attachment(
                "Candidate",
                candidate_id,
                rest_url,
                params,
                attachment,
                adapter,
                parameters,
            )
            if not attachment_response:
                failed_profiles.append(profile)
                continue

        adapter.info(
            f"Checking if candidate {candidate_id} has already applied for job"
            f" {parameters.job_id}"
        )
        job_submission_results = search_entity(
            "JobSubmission",
            rest_url,
            bh_rest_token,
            f"candidate.id:{candidate_id} AND jobOrder.id:{parameters.job_id}",
            "id,status,dateAdded",
            adapter,
            parameters,
        )

        job_submission_exists = job_submission_results.get("count", 0) > 0
        job_submission_id = (
            job_submission_results["data"][0]["id"] if job_submission_exists else None
        )

        job_submission_payload = {
            "candidate": {"id": candidate_id},
            "jobOrder": {"id": parameters.job_id},
            "status": parameters.status_when_created,
            "dateWebResponse": int(time.time() * 1000),
        }

        adapter.info("Creating or updating JobSubmission")
        job_submission_response = (
            update_entity(
                "JobSubmission",
                job_submission_id,
                rest_url,
                params,
                job_submission_payload,
                parameters,
                adapter,
            )
            if job_submission_exists
            else create_entity(
                "JobSubmission",
                rest_url,
                params,
                job_submission_payload,
                parameters,
                adapter,
            )
        )

        if not job_submission_response:
            failed_profiles.append(profile)

    return failed_profiles


def generic_job_pulling(
    action: Mode,
):
    def _pull_items(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: t.Union[
            ReadCreatedJobsCriterias,
            ReadUpdatedJobsCriterias,
            ReadArchivedJobsCriterias,
        ],
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[dict]:
        start, auth_retries, total_returned = 0, 0, 0
        should_break = False

        authentication = auth(
            auth_parameters.username,
            auth_parameters.password,
            auth_parameters.client_id,
            auth_parameters.client_secret,
        )

        if action is Mode.create:
            date_field = "created_date"
            bullhorn_date_field = "dateAdded"
        else:
            date_field = "last_modified_date"
            bullhorn_date_field = "dateLastModified"

        last_id = ""
        if not incremental:
            date_value = getattr(parameters, date_field)
            if date_value is None:
                raise Exception(f"{date_field} cannot be None in ReadMode.sync")
            last_id = None
        else:
            if incremental_token:
                try:
                    read_data = json.loads(incremental_token)
                    date_value = read_data[date_field]
                    last_id = read_data["last_id"]
                except (json.JSONDecodeError, KeyError) as e:
                    raise Exception(f"Error parsing read_from: {e}")
            else:
                date_value = getattr(parameters, date_field)
                last_id = None

        date_filter = transform_iso(date_value)
        if not date_filter:
            raise Exception(f"Error applying transformation on {date_field}")

        # Construct the query
        query = f"{bullhorn_date_field}:[{date_filter} TO *]"
        if parameters.query:
            query = f"{parameters.query} AND {query}"
        # Fetch and process jobs
        while True:
            try:
                jobs_url = f"{authentication['restUrl']}search/JobOrder"
                params = {
                    "query": query,
                    "fields": parameters.fields,
                    "sort": f"{bullhorn_date_field},id",
                    "start": str(start),
                }
                if parameters.limit:
                    params["count"] = str(parameters.limit)

                headers = {"BhRestToken": authentication["BhRestToken"]}
                response = requests.get(url=jobs_url, params=params, headers=headers)
                if response.status_code // 100 != 2:
                    adapter.error(
                        "Failed to pull jobs from Bullhorn"
                        f" status_code={response.status_code} response={response.text}"
                    )
                    raise Exception("Failed to pull jobs from Bullhorn")

                response = response.json()
                start = response["start"] + response["count"]
                data = response["data"]

                for job in data:
                    if parameters.limit and total_returned >= parameters.limit:
                        should_break = True
                        break

                    if (
                        action is Mode.create
                        and transform_timestamp_read_from(job.get("dateAdded"))[:19]
                        != transform_timestamp_read_from(job.get("dateLastModified"))[
                            :19
                        ]  # ignore microsecond difference created by Bullhorn
                    ) or (
                        action is Mode.update
                        and job.get("dateAdded") == job.get("dateLastModified")
                    ):
                        continue

                    if (
                        last_id
                        and job[bullhorn_date_field] == date_value
                        and job["id"] <= last_id
                    ):
                        adapter.info("Skipping job with id <= last_id")
                        continue
                    yield job
                    total_returned += 1

                if should_break:
                    break

                if start >= response["total"]:
                    break

            except requests.HTTPError as e:
                if e.response.status_code == 401:
                    adapter.info("Received 401 error. Retrying authentication.")
                    if auth_retries > 2:
                        raise Exception("Max auth retries exceeded")
                    authentication = auth(
                        auth_parameters.username,
                        auth_parameters.password,
                        auth_parameters.client_id,
                        auth_parameters.client_secret,
                        refresh_token=authentication["refresh_token"],
                    )
                    auth_retries += 1
                else:
                    adapter.error("Failed to fetch jobs from Bullhorn.")
                    raise e

    return _pull_items


def generic_profile_pulling(
    action: Mode,
):
    def __pull_items(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: t.Union[
            ReadCreatedProfilesCriterias,
            ReadUpdatedProfilesCriterias,
            ReadArchivedProfilesCriterias,
        ],
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[dict]:
        authentication = auth(
            auth_parameters.username,
            auth_parameters.password,
            auth_parameters.client_id,
            auth_parameters.client_secret,
        )
        start, auth_retries, total_returned = 0, 0, 0
        should_break = False

        authentication = auth(
            auth_parameters.username,
            auth_parameters.password,
            auth_parameters.client_id,
            auth_parameters.client_secret,
        )
        if action is Mode.create:
            date_field = "created_date"
            bullhorn_date_field = "dateAdded"
        else:
            date_field = "last_modified_date"
            bullhorn_date_field = "dateLastModified"

        last_id = ""
        if not incremental:
            date_value = getattr(parameters, date_field)
            if date_value is None:
                raise Exception(f"{date_field} cannot be None in ReadMode.sync")
            last_id = None
        else:
            if incremental_token:
                try:
                    read_data = json.loads(incremental_token)
                    date_value = read_data[date_field]
                    last_id = read_data["last_id"]
                except (json.JSONDecodeError, KeyError) as e:
                    raise Exception(f"Error parsing read_from: {e}")
            else:
                date_value = getattr(parameters, date_field)
                last_id = None

        date_filter = transform_iso(date_value)
        if not date_filter:
            raise Exception(f"Error applying transformation on {date_field}")

        # Construct the query
        query = f"{bullhorn_date_field}:[{date_filter} TO *]"
        if parameters.query:
            query = f"{parameters.query} AND {query}"

        while True:
            try:
                profiles_url = f"{authentication['restUrl']}search/Candidate"
                params = {
                    "query": query,
                    "fields": parameters.fields,
                    "sort": f"{bullhorn_date_field},id",
                    "start": str(start),
                }

                if parameters.limit:
                    params["count"] = str(parameters.limit)

                headers = {"BhRestToken": authentication["BhRestToken"]}

                response = requests.get(
                    url=profiles_url, params=params, headers=headers
                )
                if response.status_code // 100 != 2:
                    adapter.error(
                        "Failed to pull profiles from Bullhorn"
                        f" status_code={response.status_code} response={response.text}"
                    )
                    raise Exception("Failed to pull profiles from Bullhorn")
                response = response.json()

                start = response["start"] + response["count"]
                total = response["total"]
                data = response["data"]

                for profile in data:
                    if parameters.limit and total_returned >= parameters.limit:
                        should_break = True
                        break

                    if (
                        action is Mode.create
                        and transform_timestamp_read_from(profile.get("dateAdded"))[:19]
                        != transform_timestamp_read_from(
                            profile.get("dateLastModified")
                        )[
                            :19
                        ]  # ignore microsecond difference created by Bullhorn
                    ) or (
                        action is Mode.update
                        and profile.get("dateAdded") == profile.get("dateLastModified")
                    ):
                        continue

                    if (
                        last_id
                        and profile.get(bullhorn_date_field) == date_value
                        and profile.get("id") <= last_id
                    ):
                        adapter.info("Skipping profile with id <= last_id")
                        continue

                    if action is Mode.archive:
                        yield profile
                        total_returned += 1
                        continue

                    if parameters.parse_resume:
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
                        file_name = None
                        if len(response["EntityFiles"]) > 0:
                            for entity_file in response["EntityFiles"]:
                                if entity_file["type"] == "Resume":
                                    if not curr_entity:
                                        curr_entity = entity_file
                                        last_cv = entity_file["id"]
                                        file_name = entity_file["name"]
                                    elif (
                                        curr_entity["dateAdded"]
                                        < entity_file["dateAdded"]
                                    ):
                                        curr_entity = entity_file
                                        last_cv = entity_file["id"]
                                        file_name = entity_file["name"]

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
                            profile["fileName"] = file_name

                    if "educations" in parameters.fields:
                        education_ids = []
                        educations = []
                        for ed in profile["educations"]["data"]:
                            education_ids.append(ed["id"])
                        for id in education_ids:
                            education_url = (
                                f"{authentication['restUrl']}entity/CandidateEducation"
                                f"/{str(id)}?fields='city,school,startDate,endDate,"
                                "degree,certification,comments'"
                            )

                            response = requests.get(url=education_url, headers=headers)
                            response = response.json()
                            educations.append(response["data"])

                        profile["educations"] = educations

                    if "workHistories" in parameters.fields:
                        work_history_ids = []
                        work_histories = []
                        for work_history in profile["workHistories"]["data"]:
                            work_history_ids.append(work_history["id"])
                        for id in work_history_ids:
                            work_history_url = (
                                f"{authentication['restUrl']}entity/"
                                f"CandidateWorkHistory/{str(id)}"
                                "?fields='title,comments,startDate,endDate,companyName'"
                            )
                            response = requests.get(
                                url=work_history_url, headers=headers
                            )
                            response = response.json()
                            work_histories.append(response["data"])

                        profile["workHistories"] = work_histories

                    total_returned += 1
                    yield profile

                if should_break:
                    break

                if start >= total:
                    break

            except requests.HTTPError as e:
                if e.response.status_code == 401:
                    adapter.info(
                        "Received 401 error. Retrying authentication to continue"
                        " fetching profiles."
                    )
                    if auth_retries > 2:
                        raise Exception(
                            f"Retries exceeded for authentication ({auth_retries})."
                            " Stopping execution."
                        )

                    authentication = auth(
                        auth_parameters.username,
                        auth_parameters.password,
                        auth_parameters.client_id,
                        auth_parameters.client_secret,
                        refresh_token=authentication["refresh_token"],
                    )
                    auth_retries += 1
                    continue
                else:
                    adapter.error("Failed to fetch profiles from Bullhorn.")
                    raise e

    return __pull_items


def transform_iso(iso_date: t.Optional[t.Union[str, datetime]]) -> t.Optional[str]:
    if iso_date is None:
        return None

    if isinstance(iso_date, str):
        dt = datetime.fromisoformat(iso_date.replace("Z", "+00:00"))
    elif isinstance(iso_date, datetime):
        dt = iso_date
    else:
        raise TypeError(f"Expected str or datetime, got {type(iso_date)}")

    # Return the date formatted in the desired format
    return dt.strftime("%Y%m%d%H%M%S")


def transform_timestamp_read_from(
    timestamp: t.Optional[t.Union[float, int]],
) -> t.Optional[str]:
    if timestamp is None:
        return None
    transformed_date = datetime.utcfromtimestamp(int(timestamp) / 1000)
    return transformed_date.isoformat()


def item_to_read_from_create(item: dict) -> str:
    created_date = transform_timestamp_read_from(item["dateAdded"])
    return json.dumps(dict(created_date=created_date, last_id=item["id"]))


def item_to_read_from_update_or_archive(item: dict) -> str:
    last_modified_date = transform_timestamp_read_from(item["dateLastModified"])
    return json.dumps(dict(last_modified_date=last_modified_date, last_id=item["id"]))


ProfilesAisle = Aisle(
    name=Entity.profile,
    read=ReadOperation(
        function=merge(
            create=generic_profile_pulling(Mode.create),
            update=generic_profile_pulling(Mode.update),
            archive=generic_profile_pulling(Mode.archive),
        ),
        criterias=Criterias(
            create=ReadCreatedProfilesCriterias,
            update=ReadUpdatedProfilesCriterias,
            archive=ReadArchivedProfilesCriterias,
        ),
    ),
    schema=BullhornProfile,
)

# FIXME generic_job_pulling doesn't seem to handle the archive mode
JobsAisle = Aisle(
    name=Entity.job,
    read=ReadOperation(
        function=merge(
            create=generic_job_pulling(Mode.create),
            update=generic_job_pulling(Mode.update),
            archive=generic_job_pulling(Mode.archive),
        ),
        criterias=Criterias(
            create=ReadCreatedJobsCriterias,
            update=ReadUpdatedJobsCriterias,
            archive=ReadArchivedJobsCriterias,
        ),
    ),
    schema=BullhornJob,
)


ApplicationsAisle = Aisle(
    name=Entity.application,
    write=WriteOperation(
        function=merge(update=update_application),
        criterias=Criterias(update=UpdateApplicationsCriterias),
    ),
    schema=BullhornProfile,
)
