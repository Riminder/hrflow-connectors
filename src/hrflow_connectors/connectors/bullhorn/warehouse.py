import json
import time
import typing as t
from datetime import datetime
from io import BytesIO
from logging import LoggerAdapter

import requests
from pydantic import Field, validator

from hrflow_connectors.connectors.bullhorn.schemas import BullhornJob, BullhornProfile
from hrflow_connectors.connectors.bullhorn.utils.authentication import auth
from hrflow_connectors.core.warehouse_v2 import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)


class AuthParameters(ParametersModel):
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


# class WriteProfilesParameters(BaseParameters):
#     pass


class WriteApplicationsParameters(ParametersModel):
    job_id: str = Field(
        ...,
        description="id for the job in Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )
    # maybe should be optional
    status_when_created: str = Field(
        ...,
        description="The status of the application when created in Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )
    # maybe should not be a parameterx
    source: str = Field(
        None,
        description="The source of the application to be created in Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )


# Define a base parameters model for common fields
class BaseParameters(ParametersModel):
    limit: t.Optional[int] = Field(
        None,
        description="Number of items to pull, ignored if not provided.",
        repr=False,
        field_type=FieldType.QueryParam,
    )


# Define a base class for job parameters that includes common fields
class BaseJobsParameters(BaseParameters):
    fields: str = Field(
        (
            "address,assignedUsers,businessSectors,categories,clientBillRate,clientContact,"
            "clientCorporation,costCenter,customInt1,customInt2,customText1,customText10,"
            "customText11,customText12,customText13,customText2,customText3,customText4,"
            "customText5,customText6,customText7,customText8,customText9,customTextBlock1,"
            "customTextBlock2,customTextBlock3,customTextBlock4,customTextBlock5,dateAdded,"
            "dateEnd,degreeList,description,durationWeeks,educationDegree,employmentType,"
            "feeArrangement,hoursOfOperation,hoursPerWeek,isOpen,isWorkFromHome,markUpPercentage,"
            "numOpenings,onSite,payRate,salary,salaryUnit,skills,skillList,source,specialties,"
            "startDate,status,title,type,willRelocate"
        ),
        min_length=2,
        description="List of job fields to be retrieved from Bullhorn",
        repr=False,
        field_type=FieldType.QueryParam,
    )


class CreateJobsParameters(BaseJobsParameters):
    created_date: datetime = Field(
        ...,
        description="The creation date from which you want to pull jobs",
        repr=False,
        field_type=FieldType.QueryParam,
    )

    query: str = Field(
        "isDeleted:0 AND isOpen:true",
        description=(
            "This query will restrict the results retrieved from Bullhorn based on the"
            " specified conditions"
        ),
        repr=False,
        field_type=FieldType.QueryParam,
    )


class UpdateJobsParameters(BaseJobsParameters):
    last_modified_date: datetime = Field(
        ...,
        description="The modification date from which you want to pull jobs",
        repr=False,
        field_type=FieldType.QueryParam,
    )


# ArchiveJobsParameters can reuse the BaseParameters to avoid repeating limit
class ArchiveJobsParameters(BaseParameters):
    last_modified_date: datetime = Field(
        ...,
        description=(
            "The modification date from which you want to pull jobs and archive them"
        ),
        repr=False,
        field_type=FieldType.QueryParam,
    )

    query: str = Field(
        "isDeleted:0 AND isOpen:true",
        description=(
            "This query will restrict the results retrieved from Bullhorn based on the"
            " specified conditions"
        ),
        repr=False,
        field_type=FieldType.QueryParam,
    )

    fields: str = Field(
        "id",
        description="Field to be used as reference for archiving",
        repr=False,
        field_type=FieldType.QueryParam,
    )


# BaseProfilesParameters to manage common fields for profiles
class BaseProfilesParameters(BaseParameters):
    fields: str = Field(
        (
            "address,businessSectors,categories,companyName,customInt4,customInt5,"
            "customInt6,customText1,customText10,customText11,customText12,customText13,"
            "customText14,customText15,customText16,customText18,customText23,customText24,"
            "customText25,customText4,customText5,customText6,customText9,dateAdded,dateAvailable,"
            "dateAvailableEnd,dateLastModified,dateOfBirth,dayRate,dayRateLow,degreeList,"
            "desiredLocations,description,disability,educations,email,email2,employmentPreference,ethnicity,"
            "experience,firstName,id,lastName,mobile,name,namePrefix,occupation,owner,phone,"
            "primarySkills,secondaryOwners,secondarySkills,salary,salaryLow,skillSet,source,"
            "specialties,status,userDateAdded,veteran,willRelocate,workHistories,workPhone"
        ),
        min_length=2,
        description="List of profile fields to be retrieved from Bullhorn",
        repr=False,
        field_type=FieldType.QueryParam,
    )


class CreateProfilsParameters(BaseProfilesParameters):
    created_date: datetime = Field(
        ...,
        description="The creation date from which you want to pull profiles",
        repr=False,
        field_type=FieldType.QueryParam,
    )

    query: str = Field(
        "isDeleted:0",
        description=(
            "This query will restrict the results retrieved from Bullhorn based on the"
            " specified conditions"
        ),
        repr=False,
        field_type=FieldType.QueryParam,
    )

    parse_resume: bool = Field(
        False,
        description=(
            "If True, resumes will be retrieved and parsed along with the profile data"
        ),
        repr=False,
        field_type=FieldType.QueryParam,
    )


class UpdateProfilsParameters(BaseProfilesParameters):
    last_modified_date: datetime = Field(
        ...,
        description="The modification date from which you want to pull profiles",
        repr=False,
        field_type=FieldType.QueryParam,
    )

    parse_resume: bool = Field(
        False,
        description=(
            "If True, resumes will be retrieved and parsed along with the profile data"
        ),
        repr=False,
        field_type=FieldType.QueryParam,
    )


class ArchiveProfilsParameters(BaseParameters):
    last_modified_date: datetime = Field(
        ...,
        description="The modification date from which you want to pull profiles",
        repr=False,
        field_type=FieldType.QueryParam,
    )

    query: str = Field(
        "isDeleted:0",
        description=(
            "This query will restrict the results retrieved from Bullhorn based on the"
            " specified conditions"
        ),
        repr=False,
        field_type=FieldType.QueryParam,
    )

    fields: str = Field(
        "id",
        description="Field to be used as reference for archiving",
        repr=False,
        field_type=FieldType.QueryParam,
    )


# def create_profiles(
#     adapter: LoggerAdapter,
#     parameters: WriteProfilesParameters,
#     profiles: t.Iterable[t.Dict],
# ) -> t.List[t.Dict]:
#     adapter.info("Pushing {} profiles".format(len(profiles)))
#     failed_profiles = []
#     authentication = auth(
#         parameters.username,
#         parameters.password,
#         parameters.client_id,
#         parameters.client_secret,
#     )

#     for profile in profiles:
#         # Split profile in four parts: Body, Education, Experience and Attachements
#         profile_body_dict = profile
#         create_profile_body = profile_body_dict["create_profile_body"]
#         enrich_profile_education = profile_body_dict["enrich_profile_education"]
#         enrich_profile_experience = profile_body_dict["enrich_profile_experience"]
#         enrich_profile_attachment = profile_body_dict["enrich_profile_attachment"]

#         rest_url = authentication["restUrl"]
#         params = {"BhRestToken": authentication["BhRestToken"]}

#         candidate_url = rest_url + "entity/Candidate"
#         response = requests.put(
#             url=candidate_url, json=create_profile_body, params=params
#         )

#         # Unable to push profile
#         if response.status_code // 100 != 2:
#             adapter.error(
#                 "Failed to push profile from to Bullhorn"
#                 " status_code={} response={}".format(
#                     response.status_code, response.text
#                 )
#             )
#             failed_profiles.append(profile)
#             continue

#         candidate_id = response.json()
#         candidate_id = str(candidate_id["changedEntityId"])

#         # Enrich profile education
#         for education in enrich_profile_education:
#             education["candidate"]["id"] = candidate_id
#             education_url = rest_url + "entity/CandidateEducation"
#             response = requests.put(
#                 url=education_url,
#                 json=education,
#                 params=params
#             )

#         # Enrich profile experience
#         for experience in enrich_profile_experience:
#             experience["candidate"]["id"] = candidate_id
#             experience_url = rest_url + "entity/CandidateWorkHistory"
#             response = requests.put(
#                 url=experience_url,
#                 json=experience,
#                 params=params
#             )

#         # Enrich profile attachements
#         for attachment in enrich_profile_attachment:
#             attachment_url = rest_url + "file/Candidate/" + str(candidate_id)
#             response = requests.put(
#                 url=attachment_url,
#                 json=attachment,
#                 params=params
#             )

#     return failed_profiles


def make_request(method, url, params, auth_parameters, adapter, json=None):
    response = method(url, params=params, data=json)
    if response.status_code == 401:
        adapter.info("Auth token expired, regenerating...")
        auth_info = auth(**auth_parameters)
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
    action_parameters: WriteApplicationsParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    auth_info = auth(**auth_parameters)
    rest_url = auth_info["restUrl"]
    bh_rest_token = auth_info["BhRestToken"]
    params = {"BhRestToken": bh_rest_token}

    for profile in profiles:
        attachment = profile.pop("attachment", None)
        profile["source"] = [action_parameters.source or profile.get("source")]
        profile["status"] = action_parameters.status_when_created or profile.get(
            "status"
        )
        email = profile["email"]

        adapter.info(f"Checking if candidate with email: {email} already exists.")
        search_results = search_entity(
            "Candidate",
            rest_url,
            bh_rest_token,
            f"(email:{email} OR email2:{email}) AND isDeleted:0",
            "id,isDeleted,dateAdded,status,source,email,firstName,lastName,name,mobile,address",
            adapter,
            auth_parameters,
        )

        if not search_results:
            failed_profiles.append(profile)
            continue

        if search_results["count"] == 0:
            adapter.info(f"Creating candidate with email: {email}")
            candidate_response = create_entity(
                "Candidate", rest_url, params, profile, auth_parameters, adapter
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
                auth_parameters,
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
                    "Candidate", rest_url, params, candidate_id, adapter
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
                auth_parameters,
            )
            if not attachment_response:
                failed_profiles.append(profile)
                continue

        adapter.info(
            f"Checking if candidate {candidate_id} has already applied for job"
            f" {action_parameters.job_id}"
        )
        job_submission_results = search_entity(
            "JobSubmission",
            rest_url,
            bh_rest_token,
            f"candidate.id:{candidate_id} AND jobOrder.id:{action_parameters.job_id}",
            "id,status,dateAdded",
            adapter,
            auth_parameters,
        )

        job_submission_exists = job_submission_results.get("count", 0) > 0
        job_submission_id = (
            job_submission_results["data"][0]["id"] if job_submission_exists else None
        )

        job_submission_payload = {
            "candidate": {"id": candidate_id},
            "jobOrder": {"id": action_parameters.job_id},
            "status": action_parameters.status_when_created,
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
                auth_parameters,
                adapter,
            )
            if job_submission_exists
            else create_entity(
                "JobSubmission",
                rest_url,
                params,
                job_submission_payload,
                auth_parameters,
                adapter,
            )
        )

        if not job_submission_response:
            failed_profiles.append(profile)

    return failed_profiles


def generic_job_pulling(
    action: str,
) -> t.Callable[
    [
        LoggerAdapter,
        AuthParameters,
        t.Union[CreateJobsParameters, UpdateJobsParameters, ArchiveJobsParameters],
        t.Optional[ReadMode],
        t.Optional[str],
    ],
    t.Iterable[t.Dict],
]:
    def _pull_items(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        action_parameters: t.Union[CreateJobsParameters, UpdateJobsParameters],
        read_mode: t.Optional[ReadMode] = None,
        read_from: t.Optional[str] = None,
    ) -> t.Iterable[t.Dict]:
        start, auth_retries, total_returned = 0, 0, 0
        should_break = False

        authentication = auth(
            auth_parameters.username,
            auth_parameters.password,
            auth_parameters.client_id,
            auth_parameters.client_secret,
        )

        if action == "create":
            date_field = "created_date"
            bullhorn_date_field = "dateAdded"
        else:
            date_field = "last_modified_date"
            bullhorn_date_field = "dateLastModified"

        last_id = ""
        if read_mode is ReadMode.sync:
            date_value = getattr(action_parameters, date_field)
            if date_value is None:
                raise Exception(f"{date_field} cannot be None in ReadMode.sync")
            last_id = None
        else:
            if read_from:
                try:
                    read_data = json.loads(read_from)
                    date_value = read_data[date_field]
                    last_id = read_data["last_id"]
                except (json.JSONDecodeError, KeyError) as e:
                    raise Exception(f"Error parsing read_from: {e}")
            else:
                date_value = getattr(action_parameters, date_field)
                last_id = None

        date_filter = transform_iso(date_value)
        if not date_filter:
            raise Exception(f"Error applying transformation on {date_field}")

        # Construct the query
        query = f"{bullhorn_date_field}:[{date_filter} TO *]"
        if action in ("create", "archive") and action_parameters.query:
            query = f"{action_parameters.query} AND {query}"
        # Fetch and process jobs
        while True:
            try:
                jobs_url = f"{authentication['restUrl']}search/JobOrder"
                params = {
                    "query": query,
                    "fields": action_parameters.fields,
                    "sort": f"{bullhorn_date_field},id",
                    "start": str(start),
                }
                if action_parameters.limit:
                    params["count"] = action_parameters.limit

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
                    if (
                        action_parameters.limit
                        and total_returned >= action_parameters.limit
                    ):
                        should_break = True
                        break

                    if (
                        action == "create"
                        and job.get("dateAdded") != job.get("dateLastModified")
                    ) or (
                        action == "update"
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
                        raise Exception(f"Max auth retries exceeded")
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
    action: str,
) -> t.Callable[
    [
        LoggerAdapter,
        AuthParameters,
        t.Union[CreateJobsParameters, UpdateJobsParameters],
        t.Optional[ReadMode],
        t.Optional[str],
    ],
    t.Iterable[t.Dict],
]:
    def __pull_items(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        action_parameters: CreateProfilsParameters,
        read_mode: t.Optional[ReadMode] = None,
        read_from: t.Optional[str] = None,
    ) -> t.Iterable[t.Dict]:
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

        if action == "create":
            date_field = "created_date"
            bullhorn_date_field = "dateAdded"
        else:
            date_field = "last_modified_date"
            bullhorn_date_field = "dateLastModified"

        last_id = ""
        if read_mode is ReadMode.sync:
            date_value = getattr(action_parameters, date_field)
            if date_value is None:
                raise Exception(f"{date_field} cannot be None in ReadMode.sync")
            last_id = None
        else:
            if read_from:
                try:
                    read_data = json.loads(read_from)
                    date_value = read_data[date_field]
                    last_id = read_data["last_id"]
                except (json.JSONDecodeError, KeyError) as e:
                    raise Exception(f"Error parsing read_from: {e}")
            else:
                date_value = getattr(action_parameters, date_field)
                last_id = None

        date_filter = transform_iso(date_value)
        if not date_filter:
            raise Exception(f"Error applying transformation on {date_field}")

        # Construct the query
        query = f"{bullhorn_date_field}:[{date_filter} TO *]"
        if action in ("create", "archive") and action_parameters.query:
            query = f"{action_parameters.query} AND {query}"

        while True:
            try:
                profiles_url = f"{authentication['restUrl']}search/Candidate"
                params = {
                    "query": query,
                    "fields": action_parameters.fields,
                    "sort": f"{bullhorn_date_field},id",
                    "start": str(start),
                }

                if action_parameters.limit:
                    params["count"] = action_parameters.limit

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
                    if (
                        action_parameters.limit
                        and total_returned >= action_parameters.limit
                    ):
                        should_break = True
                        break

                    if (
                        action == "create"
                        and profile.get("dateAdded") != profile.get("dateLastModified")
                    ) or (
                        action == "update"
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
                    if action_parameters.parse_resume:
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

                    if "educations" in action_parameters.fields:
                        education_ids = []
                        educations = []
                        for ed in profile["educations"]["data"]:
                            education_ids.append(ed["id"])
                        for id in education_ids:
                            education_url = (
                                authentication["restUrl"]
                                + "entity/CandidateEducation/"
                                + str(id)
                                + "?fields='city,school,startDate,endDate,degree,certification,comments'"
                            )
                            response = requests.get(url=education_url, headers=headers)
                            response = response.json()
                            educations.append(response["data"])

                        profile["educations"] = educations

                    if "workHistories" in action_parameters.fields:
                        work_history_ids = []
                        work_histories = []
                        for work_history in profile["workHistories"]["data"]:
                            work_history_ids.append(work_history["id"])
                        for id in work_history_ids:
                            work_history_url = (
                                authentication["restUrl"]
                                + "entity/CandidateWorkHistory/"
                                + str(id)
                                + "?fields='title,comments,startDate,endDate,companyName'"
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
    timestamp: t.Optional[t.Union[float, int]]
) -> t.Optional[str]:
    if timestamp is None:
        return None
    transformed_date = datetime.utcfromtimestamp(int(timestamp) / 1000)
    return transformed_date.isoformat()


def item_to_read_from_create(item: t.Dict) -> str:
    created_date = transform_timestamp_read_from(item["dateAdded"])
    return json.dumps(dict(created_date=created_date, last_id=item["id"]))

    return json.dumps(dict(created_date=created_date, last_id=item["id"]))


def item_to_read_from_update_or_archive(item: t.Dict) -> str:
    last_modified_date = transform_timestamp_read_from(item["dateLastModified"])
    return json.dumps(dict(last_modified_date=last_modified_date, last_id=item["id"]))


BullhornCreateProfileWarehouse = Warehouse(
    name="Bullhorn Create Profils",
    data_schema=BullhornProfile,
    data_type=DataType.job,
    read=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=CreateProfilsParameters,
        function=generic_profile_pulling(action="create"),
        supports_incremental=True,
        item_to_read_from=item_to_read_from_create,
    ),
)
BullhornUpdateProfileWarehouse = Warehouse(
    name="Bullhorn Update Profils",
    data_schema=BullhornProfile,
    data_type=DataType.job,
    read=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=UpdateProfilsParameters,
        function=generic_profile_pulling(action="update"),
        supports_incremental=True,
        item_to_read_from=item_to_read_from_update_or_archive,
    ),
)
BullhornArchiveProfileWarehouse = Warehouse(
    name="Bullhorn Archive Profils",
    data_schema=BullhornProfile,
    data_type=DataType.job,
    read=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=ArchiveProfilsParameters,
        function=generic_profile_pulling(action="archive"),
        supports_incremental=True,
        item_to_read_from=item_to_read_from_update_or_archive,
    ),
)


BullhornCreateJobWarehouse = Warehouse(
    name="Bullhorn Create Jobs",
    data_schema=BullhornJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=CreateJobsParameters,
        function=generic_job_pulling(action="create"),
        supports_incremental=True,
        item_to_read_from=item_to_read_from_create,
    ),
)

BullhornUpdateJobWarehouse = Warehouse(
    name="Bullhorn Update Jobs",
    data_schema=BullhornJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=UpdateJobsParameters,
        function=generic_job_pulling(action="update"),
        supports_incremental=True,
        item_to_read_from=item_to_read_from_update_or_archive,
    ),
)

BullhornArchiveJobWarehouse = Warehouse(
    name="Bullhorn Archive Jobs",
    data_schema=BullhornJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=ArchiveJobsParameters,
        function=generic_job_pulling(action="archive"),
        supports_incremental=True,
        item_to_read_from=item_to_read_from_update_or_archive,
    ),
)

BullhornApplicationWarehouse = Warehouse(
    name="Bullhorn Applications",
    data_schema=BullhornProfile,
    data_type=DataType.profile,
    update=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=WriteApplicationsParameters,
        function=update_application,
        endpoints=[],
    ),
)
