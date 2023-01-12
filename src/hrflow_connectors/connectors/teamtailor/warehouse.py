import enum
import json
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.teamtailor.schema import (
    TeamtailorCandidateAttribute,
    TeamtailorJob,
)
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

GET_ALL_JOBS_ENDPOINT = "https://api.teamtailor.com/v1/jobs"
GET_JOB_ENDPOINT = "https://api.teamtailor.com/v1/jobs"
POST_CANDIDATE_ENDPOINT = "https://api.teamtailor.com/v1/candidates"


class RemoteStatus(str, enum.Enum):
    none = "NONE"
    hybrid = "HYBRID"
    temporary = "TEMPORARY"
    fully = "FULLY"


class JobFeed(str, enum.Enum):
    public = "PUBLIC"
    private = "PRIVATE"


class WriteProfilesParameters(ParametersModel):
    Authorization: str = Field(
        ...,
        description="Authorisation used to access Teamtailor API",
        repr=False,
        field_type=FieldType.Auth,
    )

    X_Api_Version: str = Field(
        ..., description="Dated version of the API", field_type=FieldType.Other
    )


class ReadJobsParameters(ParametersModel):
    Authorization: str = Field(
        ...,
        description="Authorisation token used to access Teamtailor API",
        repr=False,
        field_type=FieldType.Auth,
    )

    X_Api_Version: str = Field(
        ..., description="Dated version of the API", field_type=FieldType.Other
    )

    filter_status: t.Optional[RemoteStatus] = Field(
        None,
        description="Posting status of a job. One of {}".format(
            [e.value for e in RemoteStatus]
        ),
        field_type=FieldType.QueryParam,
    )
    filter_feed: t.Optional[JobFeed] = Field(
        None,
        description="Status of a job. One of {}".format([e.value for e in JobFeed]),
        field_type=FieldType.QueryParam,
    )


def enrich_location(job_id: str, headers: t.Dict) -> t.Dict:
    """
    Get_location sends a request to get the job location from its API endpoint
    """
    response = requests.request(
        "GET",
        "https://api.teamtailor.com/v1/jobs/{}/location".format(job_id),
        headers=headers,
        data={},
    )

    if not response.ok:
        raise Exception(
            response,
            message="Failed to get job location from Teamtailor.",
        )

    location = response.json().get("data")
    if location is not None:
        location_attribute = location.get("attributes")
        text = location_attribute["address"]
        city = location_attribute["city"]
        country = location_attribute["country"]
        headquarters = location_attribute["headquarters"]
        lat = location_attribute["lat"]
        lng = location_attribute["long"]
        zip = location_attribute["zip"]
        name = location_attribute["name"]
        geojson = dict(
            city=city,
            country=country,
            headquarters=headquarters,
            zip=zip,
            name=name,
        )
        location_obj = dict(text=text, geojson=geojson, lat=lat, lng=lng)
        return location_obj
    return dict(text="", geojson="", lat="", lng="")


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    headers = {
        "Authorization": f"Token token={parameters.Authorization}",
        "X-Api-Version": parameters.X_Api_Version,
    }

    params = dict()
    params["filter[remote-status]"] = parameters.filter_status
    params["filter[feed]"] = parameters.filter_feed

    url = GET_ALL_JOBS_ENDPOINT
    all_jobs = []
    while True:
        response = requests.request("GET", url, headers=headers, data={}, params=params)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Teamtailor status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from Teamtailor")
        response = response.json()
        jobs = response.get("data")

        all_jobs += jobs
        if response.get("links").get("next") is None:
            break
        else:
            url = response.get("links").get("next")

    adapter.info("Pulling {} jobs from Teamtailor API".format(len(all_jobs)))

    for job in all_jobs:
        full_job_response = requests.request(
            "GET",
            "{}/{}".format(GET_JOB_ENDPOINT, job.get("id")),
            headers=headers,
            data={},
        )

        if full_job_response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs details from Teamtailor job_id={}"
                " status_code={} response={}".format(
                    job["id"],
                    full_job_response.status_code,
                    full_job_response.text,
                )
            )
            raise Exception("Failed to pull jobs from Teamtailor")

        # Enrich full_job_response
        job_location = enrich_location(job.get("id"), headers)
        job_and_location = dict(job=full_job_response.json(), job_location=job_location)
        yield job_and_location


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} profiles".format(len(profiles)))
    failed_profiles = []

    for profile in profiles:
        headers = {
            "Authorization": f"Token token={parameters.Authorization}",
            "X-Api-Version": parameters.X_Api_Version,
            "Content-Type": "application/vnd.api+json",
        }
        response = requests.request(
            "POST", POST_CANDIDATE_ENDPOINT, headers=headers, data=json.dumps(profile)
        )

        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Teamtailor, "
                " status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


TeamtailorJobWarehouse = Warehouse(
    name="Teamtailor Jobs",
    data_schema=TeamtailorJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[],
    ),
)

TeamtailorProfileWarehouse = Warehouse(
    name="Teamtailor Profiles",
    data_schema=TeamtailorCandidateAttribute,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[],
    ),
)
