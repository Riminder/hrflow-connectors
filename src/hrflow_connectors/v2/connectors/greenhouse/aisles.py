import base64
import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.greenhouse.schemas import (
    GreenhouseJobModel,
    GreenhouseProfileModel,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    Endpoint,
    Endpoints,
    ReadOperation,
    WriteOperation,
    merge,
)

GREENHOUSE_API_URL = "https://harvest.greenhouse.io/v1"
GET_JOB_ENDPOINT = Endpoint(
    name="Get job",
    description=(
        "Endpoint to get the content of a job with a given id. The request method is"
        " `GET`"
    ),
    url="https://developers.greenhouse.io/harvest.html?shell#get-retrieve-job",
)
POST_CANDIDATE_ENDPOINT = Endpoint(
    name="Post Candidate",
    description=(
        "Endpoint to create a new candidate and assign to a talent pool, the request"
        " method is `POST`"
    ),
    url="https://developers.greenhouse.io/job-board.html#jobs",
)


class AuthParameters(Struct):
    auth: Annotated[str, Meta(description="XAPIKeyAuth")]


class WriteProfilesParameters(Struct):
    on_behalf_of: Annotated[
        str,
        Meta(
            description=(
                "ID of the user issuing this request. Required for auditing purposes."
            ),
        ),
    ]


class ReadProfilesParameters(Struct):
    # TODO: verify pagination (page and per_page usage) in the function
    skip_count: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "If true, the performance of retrieving candidates will improve. This"
                " will remove last from the link response header."
            ),
        ),
    ] = None
    created_before: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Return only candidates that were created before this timestamp."
                " Timestamp must be in in ISO-8601 format."
            ),
        ),
    ] = None
    created_after: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Return only candidates that were created at or after this timestamp."
                " Timestamp must be in in ISO-8601 format."
            ),
        ),
    ] = None
    updated_before: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Return only candidates that were updated before this timestamp."
                " Timestamp must be in in ISO-8601 format."
            ),
        ),
    ] = None
    updated_after: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Return only candidates that were updated at or after this timestamp."
                " Timestamp must be in in ISO-8601 format."
            ),
        ),
    ] = None
    job_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Only returns candidates who have applied to the specified job."
                " Prospects on the job are not included."
            ),
        ),
    ] = None
    email: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "If supplied, only return candidates who have a matching e-mail"
                " address. If supplied with job_id, only return a candidate with a"
                " matching e-mail with an application on the job. If email and"
                " candidate_ids are included, candidate_ids will be ignored."
            ),
        ),
    ] = None
    candidate_ids: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "If supplied, return only the candidates with the given ids. These are"
                " supplied as a comma separated string. e.g.:"
                " 'candidate_ids=123,456,789'. When combined with job_id, only return"
                " candidates with an application on the job. A maximum of 50 candidates"
                " can be returned this way."
            ),
        ),
    ] = None


class ReadJobsParameters(Struct, omit_defaults=True):
    # TODO: verify pagination (page and per_page usage) in the function
    skip_count: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "If true, the performance of retrieving jobs will improve. This will"
                " remove last from the link response header."
            ),
        ),
    ] = None
    created_before: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Return only jobs that were created before this timestamp. Timestamp"
                " must be in in ISO-8601 format."
            ),
        ),
    ] = None
    created_after: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Return only jobs that were created at or after this timestamp."
                " Timestamp must be in in ISO-8601 format."
            ),
        ),
    ] = None
    updated_before: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Return only jobs that were updated before this timestamp. Timestamp"
                " must be in in ISO-8601 format."
            ),
        ),
    ] = None
    updated_after: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Return only jobs that were updated at or after this timestamp."
                " Timestamp must be in in ISO-8601 format."
            ),
        ),
    ] = None
    requisition_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "If included, will return only the jobs that match the given"
                " requisition_id"
            ),
        ),
    ] = None
    opening_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "If included, will return only the jobs that contain at least one"
                " opening with the given opening_id."
            ),
        ),
    ] = None
    status: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "One of 'open', 'closed', or 'draft'. If included, will only return"
                " jobs with that status."
            ),
        ),
    ] = None
    department_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "If included, will return only the jobs in this specific department."
            ),
        ),
    ] = None
    external_department_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "This may be used instead of department_id and represents the ID of"
                " the department in an external system."
            ),
        ),
    ] = None
    office_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "If included, will return only the jobs in this specific office."
            ),
        ),
    ] = None
    external_office_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "This may be used instead of office_id and represents the ID of the"
                " office in an external system."
            ),
        ),
    ] = None
    custom_field_option_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The job contains a custom field with this custom_field_option_id"
                " selected. Option IDs can be retrieved from the GET Custom Field"
                " Options endpoint."
            ),
        ),
    ] = None


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    auth_parameters.auth = auth_parameters.auth + ":"
    authorization = base64.b64encode(auth_parameters.auth.encode("ascii"))
    page = 0
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    all_profiles = []
    while True:
        params["page"] = page
        response = requests.get(
            "{}/candidates".format(GREENHOUSE_API_URL),
            headers={
                "Authorization": b"Basic " + authorization,
            },
            params=params,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profiles from Greenhouse params={}"
                " status_code={} response={}".format(
                    parameters, response.status_code, response.text
                )
            )
            raise Exception("Failed to pull profiles from Greenhouse")
        response = response.json()
        profiles = response.get("candidates", [])
        if len(profiles) == 0:
            break
        all_profiles += profiles

        page += 1

    adapter.info("Pulling {} profiles".format(len(all_profiles)))
    for profile in all_profiles:
        yield profile


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    auth_parameters.auth = auth_parameters.auth + ":"
    authorization = base64.b64encode(auth_parameters.auth.encode("ascii"))

    page = 0
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    all_jobs = []
    while True:
        params["page"] = page
        response = requests.get(
            "{}/jobs".format(
                GREENHOUSE_API_URL,
            ),
            headers={
                "Authorization": b"Basic " + authorization,
            },
            params=params,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Greenhouse params={}"
                " status_code={} response={}".format(
                    parameters, response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from Greenhouse")
        response = response.json()
        jobs = response.get("jobs", [])
        if len(jobs) == 0:
            break
        all_jobs += jobs

        page += 1

        adapter.info("Pulling {} jobs".format(len(all_jobs)))
        for job in all_jobs:
            yield job
        break


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    auth_parameters.auth = auth_parameters.auth + ":"
    authorization = base64.b64encode(auth_parameters.auth.encode("ascii"))

    for profile in items:
        response = requests.post(
            "{}/candidates".format(GREENHOUSE_API_URL),
            headers={
                "On-Behalf-Of": parameters.on_behalf_of,
                "Authorization": b"Basic " + authorization,
            },
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Greenhouse status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


def edit_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    auth_parameters.auth = auth_parameters.auth + ":"
    authorization = base64.b64encode(auth_parameters.auth.encode("ascii"))

    for profile in items:
        profile_id = profile.pop("id")
        response = requests.patch(
            "{}/candidates/{}".format(GREENHOUSE_API_URL, profile_id),
            headers={
                "On-Behalf-Of": parameters.on_behalf_of,
                "Authorization": b"Basic " + authorization,
            },
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Greenhouse status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


def delete_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    auth_parameters.auth = auth_parameters.auth + ":"
    authorization = base64.b64encode(auth_parameters.auth.encode("ascii"))

    for profile in items:
        response = requests.delete(
            "{}/candidates/{}".format(GREENHOUSE_API_URL, profile["id"]),
            headers={
                "On-Behalf-Of": parameters.on_behalf_of,
                "Authorization": b"Basic " + authorization,
            },
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Greenhouse status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


JobsAisle = Aisle(
    name=Entity.job,
    schema=GreenhouseJobModel,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(create=read_jobs, update=read_jobs, archive=read_jobs),
        endpoints=Endpoints(create=GET_JOB_ENDPOINT),
    ),
)

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=GreenhouseProfileModel,
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=WriteProfilesParameters,
            archive=WriteProfilesParameters,
        ),
        function=merge(
            create=write_profiles, update=edit_profiles, archive=delete_profiles
        ),
        endpoints=Endpoints(create=POST_CANDIDATE_ENDPOINT),
    ),
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read_profiles, update=read_profiles, archive=read_profiles
        ),
    ),
)
