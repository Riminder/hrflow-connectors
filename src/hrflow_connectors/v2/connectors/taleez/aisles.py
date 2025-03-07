import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.taleez.schemas import (
    Candidate,
    ContractType,
    Job,
    JobStatus,
    JobVisibility,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

TALEEZ_PROFILES_ENDPOINT = "https://api.taleez.com/0/candidates"
TALEEZ_JOBS_ENDPOINT = "https://api.taleez.com/0/jobs"
TALEEZ_JOBS_ENDPOINT_LIMIT = 100
ACCEPT = "application/json;charset=UTF-8"
CONTENT_TYPE = "application/json"


class AuthParameters(Struct):
    x_taleez_api_secret: Annotated[
        str,
        Meta(
            description="X-taleez-api-secret used to access Taleez API",
        ),
    ]


class ReadJobsParameters(Struct, omit_defaults=True):
    unitId: Annotated[
        t.Optional[t.List[int]],
        Meta(
            description="Filter on job unit. Can be used with others filters.",
        ),
    ] = None
    status: Annotated[
        t.Optional[t.List[JobStatus]],
        Meta(
            description=(
                "Filter on job status. Can be used with others filters.\nAvailable"
                " values : {}".format([e.value for e in JobStatus])
            ),
        ),
    ] = None
    contract: Annotated[
        t.Optional[t.List[ContractType]],
        Meta(
            description="Filter on job contract. Can be used with others filters.",
        ),
    ] = None
    city: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="Filter on job city. Can be used with others filters.",
        ),
    ] = None
    companyLabel: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description=(
                "Filter on job company label (strict search). Can be used with others"
                " filters."
            ),
        ),
    ] = None
    tag: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="Filter on job tag. Can be used with others filters.",
        ),
    ] = None
    visibility: Annotated[
        t.Optional[t.List[JobVisibility]],
        Meta(
            description="Filter on job visibility. Can be used with others filters.",
        ),
    ] = None
    visibilityToken: Annotated[
        t.Optional[str],
        Meta(
            description="Secret token for restricted jobs.",
        ),
    ] = None
    sort: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Sort the list by one or multiple params. Ex :"
                " sort=dateCreation.desc,label.asc"
            ),
        ),
    ] = None


class ReadProfilesParameters(Struct, omit_defaults=True):
    mail: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="Filter by mail",
        ),
    ] = None


class WriteProfilesParameters(Struct):
    recruiterId: Annotated[
        t.Optional[int],
        Meta(
            description="Id of the recruiter to associate the candidate with",
        ),
    ] = None
    unitId: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Id of the unit associated with the candidate. Only specified for"
                ' companies with multiple units and "candidate segmentation by unit"'
                " setting."
            )
        ),
    ] = None
    job_ids: Annotated[
        t.Optional[t.List[int]],
        Meta(
            description="List of job ids to associate the candidate with",
        ),
    ] = None
    pool_ids: Annotated[
        t.Optional[t.List[int]],
        Meta(
            description="List of pool ids to associate the candidate with",
        ),
    ] = None


class UpdateProfilesParameters(Struct):
    pass


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    params["withDetails"] = True
    params["withProps"] = True
    page = 0
    jobs = []

    while True:
        params["page"] = page
        response = requests.get(
            TALEEZ_JOBS_ENDPOINT,
            headers={"X-taleez-api-secret": auth_parameters.x_taleez_api_secret},
            params=params,
        )

        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Taleez params={} status_code={} response={}"
                .format(params, response.status_code, response.text)
            )
            raise Exception("Failed to pull jobs from Taleez")

        response = response.json()
        jobs.extend(response["list"])
        if response["hasMore"] is False:
            break
        page += 1

    for job in jobs:
        yield job


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    params["withProps"] = True
    page = 0
    profiles = []

    while True:
        params["page"] = page
        response = requests.get(
            TALEEZ_PROFILES_ENDPOINT,
            headers={"X-taleez-api-secret": auth_parameters.x_taleez_api_secret},
            params=params,
        )

        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profiles from Taleez params={} status_code={}"
                " response={}".format(params, response.status_code, response.text)
            )
            raise Exception("Failed to pull profiles from Taleez")

        response = response.json()
        profiles.extend(response["list"])
        if response["hasMore"] is False:
            break
        page += 1

    for profile in profiles:
        resume_link = profile.get("resume")
        if resume_link:
            resume = requests.get(resume_link)
            if resume.status_code // 100 == 2:
                profile["resume"] = resume.content
        yield profile


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    items = list(items)
    adapter.info("Pushing {} candidates to Taleez API".format(len(items)))
    failed_profiles = []

    for profile_item in items:
        candidate = profile_item.pop("candidate")
        resume_link = profile_item.pop("resume")
        properties = profile_item.pop("properties", None)

        post_profile_response = requests.post(
            TALEEZ_PROFILES_ENDPOINT,
            headers={"X-taleez-api-secret": auth_parameters.x_taleez_api_secret},
            json=candidate,
        )
        if post_profile_response.status_code // 100 != 2:
            adapter.error(
                "Failed to create candidate status_code={} post_profile_response={}"
                .format(
                    post_profile_response.status_code,
                    post_profile_response.text,
                )
            )
            failed_profiles.append(profile_item)
            continue

        id = post_profile_response.json()["id"]
        binary_resume = requests.get(resume_link).content
        post_resume_response = requests.post(
            "{}/{}/documents".format(TALEEZ_PROFILES_ENDPOINT, id),
            headers={
                "X-taleez-api-secret": auth_parameters.x_taleez_api_secret,
            },
            params={"cv": True},
            files={
                "file": (
                    "resume_{}_{}".format(
                        candidate["firstName"],
                        candidate["lastName"],
                    ),
                    binary_resume,
                    "application/pdf",
                )
            },
        )
        if post_resume_response.status_code // 100 != 2:
            adapter.error(
                "Failed to add resume status_code={} post_resume_response={}".format(
                    post_resume_response.status_code,
                    post_resume_response.text,
                )
            )

        if properties:
            post_properties_response = requests.post(
                "{}/{}/properties".format(TALEEZ_PROFILES_ENDPOINT, id),
                headers={
                    "X-taleez-api-secret": auth_parameters.x_taleez_api_secret,
                },
                json=properties,
            )
            if post_properties_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to update property status_code={}"
                    " post_properties_response={}".format(
                        post_properties_response.status_code,
                        post_properties_response.text,
                    )
                )

        if parameters.job_ids:
            associate_job_response = requests.post(
                "{}/{}/jobs".format(TALEEZ_PROFILES_ENDPOINT, id),
                headers={
                    "X-taleez-api-secret": auth_parameters.x_taleez_api_secret,
                },
                json={"ids": parameters.job_ids},
            )
            if associate_job_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to associate candidate to jobs status_code={}"
                    " associate_job_response={}".format(
                        associate_job_response.status_code, associate_job_response.text
                    )
                )
        if parameters.pool_ids:
            associate_pool_response = requests.post(
                "{}/{}/pools".format(TALEEZ_PROFILES_ENDPOINT, id),
                headers={
                    "X-taleez-api-secret": auth_parameters.x_taleez_api_secret,
                },
                json={"ids": parameters.pool_ids},
            )
            if associate_pool_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to associate candidate to pools status_code={}"
                    " associate_pool_response={}".format(
                        associate_pool_response.status_code,
                        associate_pool_response.text,
                    )
                )

    return failed_profiles


def update_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []

    for profile_item in items:
        update_profile_response = requests.post(
            "{}/{}".format(TALEEZ_PROFILES_ENDPOINT, profile_item["id"]),
            headers={"X-taleez-api-secret": auth_parameters.x_taleez_api_secret},
            json=profile_item["candidate"],
        )
        if update_profile_response.status_code // 100 != 2:
            adapter.error(
                "Failed to update candidate status_code={} response={}".format(
                    update_profile_response.status_code,
                    update_profile_response.text,
                )
            )
            failed_profiles.append(profile_item)
            continue

        if profile_item.get("properties"):
            update_properties_response = requests.post(
                "{}/{}/properties".format(TALEEZ_PROFILES_ENDPOINT, profile_item["id"]),
                headers={"X-taleez-api-secret": auth_parameters.x_taleez_api_secret},
                json=profile_item["properties"],
            )
            if update_properties_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to update properties status_code={} response={}".format(
                        update_properties_response.status_code,
                        update_properties_response.text,
                    )
                )

    return failed_profiles


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=Candidate,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read_profiles,
            update=read_profiles,
            archive=read_profiles,
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=UpdateProfilesParameters,
        ),
        function=merge(
            create=write_profiles,
            update=update_profiles,
        ),
    ),
)
JobsAisle = Aisle(
    name=Entity.job,
    schema=Job,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=read_jobs,
            update=read_jobs,
            archive=read_jobs,
        ),
    ),
)
