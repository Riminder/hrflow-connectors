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
CANDIDATE_ENDPOINT = "https://api.teamtailor.com/v1/candidates"


class RemoteStatus(str, enum.Enum):
    none = "NONE"
    hybrid = "HYBRID"
    temporary = "TEMPORARY"
    fully = "FULLY"


class JobFeed(str, enum.Enum):
    public = "PUBLIC"
    private = "PRIVATE"


class ReadProfilesParameters(ParametersModel):
    api_token: str = Field(
        ...,
        description="Authorisation token used to access Teamtailor API",
        repr=False,
        field_type=FieldType.Auth,
    )

    x_api_version: str = Field(
        ..., description="Dated version of the API", field_type=FieldType.Auth
    )

    filter_updated_at_from: t.Optional[str] = Field(
        description="Filter candidates by created-at newer than this date.",
        field_type=FieldType.QueryParam,
    )

    @property
    def get_authorization(self) -> str:
        return f"Token token={self.api_token}"


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


def read_jobs(
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


def write_profile(
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


def download_file(url):
    response = requests.get(url)
    if response.ok:
        return response.content
    else:
        return None


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    params = dict()

    if read_mode == ReadMode.incremental:
        if read_from:
            params["filter[updated_at][from]"] = read_from
    else:
        if parameters.filter_updated_at_from:
            params["filter[updated_at][from]"] = parameters.filter_updated_at_from

    headers = {
        "Authorization": parameters.get_authorization,
        "X-Api-Version": parameters.x_api_version,
    }

    page = 1
    while True:
        response = requests.get(
            CANDIDATE_ENDPOINT,
            headers=headers,
            params=params,
            timeout=10,
        )

        if not response.ok:
            error_detail = response.json()["errors"][0]["detail"]
            raise Exception(
                f"Error while fetching candidates with params: {params} and error"
                f" {error_detail}"
            )

        data = response.json()["data"]

        if not data:
            break

        adapter.info(
            "Pulling {} candidates from page {} out of total {} candidates".format(
                len(data), page, response.json()["meta"]["record-count"]
            )
        )

        for candidate in data:
            # Get candidate resume using the id since each resume
            # Link is signed and available for 30sec only
            candidate_id = candidate["id"]
            candidate_response = requests.get(
                f"{CANDIDATE_ENDPOINT}/{candidate_id}",
                headers=headers,
                timeout=10,
            )
            if not candidate_response.ok:
                error_detail = candidate_response.json()["errors"][0]["detail"]
                raise Exception(
                    f"Error while fetching candidate {candidate_id} with error"
                    f" {error_detail}"
                )

            candidate_updated_at = candidate_response.json()["data"]["attributes"][
                "updated-at"
            ]
            candidate_resume_url = candidate_response.json()["data"]["attributes"][
                "original-resume"
            ]
            if candidate_resume_url:
                binary_resume_content = download_file(candidate_resume_url)

                # TODO : this can be put outside in format function
                resume = dict(
                    raw=binary_resume_content,
                    content_type="application/pdf",
                )
                profile = dict(
                    reference=candidate_id,
                    created_at=candidate["attributes"]["created-at"],
                    updated_at=candidate_updated_at,
                    resume=resume,
                    tags=[],
                    metadatas=[],
                )
                yield profile

        page += 1
        params["page"] = page


TeamtailorJobWarehouse = Warehouse(
    name="Teamtailor Jobs",
    data_schema=TeamtailorJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        endpoints=[],
    ),
)

TeamtailorProfileWarehouse = Warehouse(
    name="Teamtailor Profiles",
    data_schema=TeamtailorCandidateAttribute,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write_profile,
        endpoints=[],
    ),
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read_profiles,
        endpoints=[],
        supports_incremental=True,
        item_to_read_from=lambda profile: profile["updated_at"],
    ),
)
