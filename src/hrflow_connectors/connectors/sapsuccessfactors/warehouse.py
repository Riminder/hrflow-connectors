import base64
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.sapsuccessfactors.schemas import (
    SapCandidateModel,
    SAPSuccessFactorsJob,
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

SAP_JOBS_ENDPOINT_LIMIT = 100


class WriteProfilesParameters(ParametersModel):
    api_server: str = Field(
        ..., description="Server to be accessed", repr=False, field_type=FieldType.Other
    )
    api_key: str = Field(
        ...,
        description="API Key used to authenticate on the SAP API",
        field_type=FieldType.Auth,
    )


class ReadProfilesParameters(ParametersModel):
    api_server: str = Field(
        ..., description="Server to be accessed", repr=False, field_type=FieldType.Other
    )
    api_key: str = Field(
        ...,
        description="API Key used to authenticate on the SAP API",
        field_type=FieldType.Auth,
    )
    top: t.Optional[int] = Field(
        SAP_JOBS_ENDPOINT_LIMIT,
        description="Show only the first N items value is capped at {}".format(
            SAP_JOBS_ENDPOINT_LIMIT
        ),
        field_type=FieldType.QueryParam,
    )
    skip: t.Optional[int] = Field(
        description="Search items by search phrases", field_type=FieldType.QueryParam
    )
    filter: t.Optional[str] = Field(
        description="Filter items by property values",
        repr=False,
        field_type=FieldType.QueryParam,
    )
    search: t.Optional[str] = Field(
        description="Search items by search phrases",
        repr=False,
        field_type=FieldType.QueryParam,
    )


class ReadJobsParameters(ParametersModel):
    api_server: str = Field(
        ..., description="Server to be accessed", repr=False, field_type=FieldType.Other
    )
    api_key: str = Field(
        ...,
        description="API Key used to authenticate on the SAP API",
        field_type=FieldType.Auth,
    )
    top: t.Optional[int] = Field(
        SAP_JOBS_ENDPOINT_LIMIT,
        description="Show only the first N items value is capped at {}".format(
            SAP_JOBS_ENDPOINT_LIMIT
        ),
        field_type=FieldType.QueryParam,
    )
    skip: t.Optional[int] = Field(
        description="Search items by search phrases", field_type=FieldType.QueryParam
    )
    filter: t.Optional[str] = Field(
        description="Filter items by property values",
        repr=False,
        field_type=FieldType.QueryParam,
    )
    search: t.Optional[str] = Field(
        description="Search items by search phrases",
        repr=False,
        field_type=FieldType.QueryParam,
    )


# enriches a job object with it's requisition field
def enrich_requisition(parameters: ReadJobsParameters, job: t.Dict) -> t.Dict:
    job_and_requisition = dict()
    job_and_requisition["job"] = job

    url = job.get("jobRequisition").get("__deferred").get("uri")
    requisition = requests.get(
        url, headers={"APIKey": parameters.api_key, "Accept": "application/json"}
    )
    requisition = requisition.json()
    job_and_requisition["requisition"] = requisition.get("d")
    return job_and_requisition


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    params = dict()
    params["top"] = parameters.top
    params["skip"] = parameters.skip
    params["filter"] = parameters.filter
    params["search"] = parameters.search

    url = f"https://{parameters.api_server}/odata/v2/JobRequisitionLocale?%24top=20"

    response = requests.get(
        url,
        headers={"APIKey": parameters.api_key, "Accept": "application/json"},
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull jobs from SAP, status_code={}".format(response.status_code)
        )
        raise Exception("Failed to pull jobs from SAP")
    response = response.json()
    job_list = response.get("d").get("results")
    job_res = []
    for job in job_list:
        job_res.append(enrich_requisition(parameters, job))
    return job_res


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} profiles".format(len(profiles)))
    failed_profiles = []
    for profile in profiles:
        response = requests.post(
            "https://{}/odata/v2/Candidate".format(parameters.api_server),
            headers={"APIKey": parameters.api_key},
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to SAP status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    # Set the API endpoint URL
    endpoint_url = f"{parameters.api_server}/odata/v2/Candidate"

    # Set the headers with the API key
    headers = {
        "APIKey": parameters.api_key,
        "Accept": "application/json",
    }

    # Set the parameters for the API call
    params = dict()
    params["$top"] = parameters.top
    params["$skip"] = parameters.skip
    params["$filter"] = parameters.filter
    params["$search"] = parameters.search

    # Make the GET request
    response = requests.get(endpoint_url, headers=headers, params=params)

    # Check if the request was successful
    if response.status_code != 200:
        adapter.error(f"Failed to retrieve candidates. Response: {response.text}")
        raise Exception(f"Failed to retrieve candidates. Response: {response.text}")

    # Extract the candidate data from the response
    candidates = response.json()["d"]["results"]
    candidates_data = []
    for candidate in candidates:
        # Retrieve the resume
        resume = dict()
        resume_url = candidate["resume"]["__deferred"]["uri"]
        resume_response = requests.get(resume_url, headers=headers)
        file_content = resume_response.json()["d"]["fileContent"]
        resume["raw"] = base64.b64decode(file_content)
        resume["content_type"] = resume_response.json()["d"]["mimeType"]
        # Retrieve the tags
        tags_url = candidate["tags"]["__deferred"]["uri"]
        tags_response = requests.get(tags_url, headers=headers)
        tags = tags_response.json()["d"]["results"]
        candidates_data.append(
            dict(
                candidateId=candidate["candidateId"],
                resume=resume,
                tags=tags,
                metadata=candidate["__metadata"],
            )
        )

    return candidates_data


SAPProfileWarehouse = Warehouse(
    name="SAP Profiles",
    data_schema=SapCandidateModel,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[],
    ),
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read_profiles,
        endpoints=[],
    ),
)

SAPJobWarehouse = Warehouse(
    name="SAP Job",
    data_schema=SAPSuccessFactorsJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[],
    ),
)
