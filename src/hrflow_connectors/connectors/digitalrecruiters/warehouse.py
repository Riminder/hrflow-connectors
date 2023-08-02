import enum
import requests
import typing as t
from logging import LoggerAdapter
from pydantic import Field
from hrflow_connectors.connectors.digitalrecruiters.schema import DigitalRecruitersJob, DigitalRecruitersImportCandidateProfile
from hrflow_connectors.core import (
    ActionEndpoints,
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

DIGITAL_RECRUITERS_JOBS_ENDPOINT = "{environment_url}/export/job-ads/{token}"
DIGITAL_RECRUITERS_PROFILES_ENDPOINT = "{digital_recruiters_environment_url}/api/candidate/apply/{token}"
class ReadJobsParameters(ParametersModel):
    digital_recruiters_token: str = Field(
        ...,
        description="Digital Recruiters API token.",
        repr=False,
        field_type=FieldType.Auth,
    )
    digital_recruiters_environment_url: str = Field(
		...,
		description="Digital Recruiters API url environnement.",
        repr=False,
        field_type=FieldType.Other,
	)	

class WriteProfilesParameters(ParametersModel):
    digital_recruiters_token: str = Field(
        ...,
        description="Digital Recruiters API token.",
        repr=False,
        field_type=FieldType.Auth,
    )
    digital_recruiters_environment_url: str = Field(
		...,
		description="Digital Recruiters API url environnement.",
        repr=False,
        field_type=FieldType.Other,
	)
    job_reference: str = Field(
        ...,
        description="reference of the job to which the candidate is applying.",
		repr=False,
		field_type=FieldType.Other,
	)
    message: t.Optional[str] = Field(
        "message du candidat",
        description="Application message.",
        repr=False,
        field_type=FieldType.Other,
    )
			

def remove_trailing_slash(url: str) -> str:
    """
    Check if the given URL ends with a slash. If yes, remove the slash and return the modified URL.
    If not, return the original URL.

    Args:
        url (str): The URL to check.

    Returns:
        str: The modified URL without the trailing slash if it had one, otherwise the original URL.
    """
    if url.endswith("/"):
        return url[:-1]
    return url

def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    DR_JOB_URL = remove_trailing_slash(parameters.digital_recruiters_environment_url)
    url = DIGITAL_RECRUITERS_JOBS_ENDPOINT.format(environment_url=DR_JOB_URL, token=parameters.digital_recruiters_token)
    adapter.info(f"Pulling jobs from Digital Recruiters")	
    response = requests.get(url)
    if response.status_code != 200:
        adapter.error(
			f"Failed to pull jobs from Digital Recruiters. Status code: {response.status_code}."
		)
        return
    jobs = response.json().get("ads", [])
    adapter.info(f"Found {len(jobs)} jobs in Digital Recruiters.")
    if not jobs:
        adapter.warning("No jobs found in Digital Recruiters.")
        return	
    for job in jobs:
        yield job


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    DR_PROFILES_URL = remove_trailing_slash(parameters.digital_recruiters_environment_url)
    url = DIGITAL_RECRUITERS_PROFILES_ENDPOINT.format(url_environnement=DR_PROFILES_URL, token=parameters.digital_recruiters_token)	

    failed_profiles = []

    for profile in profiles:
        json_data = profile
        json_data["reference"] = parameters.job_reference
        json_data["ApplicationMessage"] = dict(
                              message=parameters.message  # You can customize this message if needed
                              )
        try:
            response = requests.post(url,json=json_data)
            response.raise_for_status()

            if response.status_code == 201:
                adapter.info("Candidate profile pushed successfully.")
            elif response.status_code == 202:
                adapter.warning("Candidate profile pushed, but some information is missing.")
            else:
                adapter.error("Failed to push candidate profile. Status code: %s, Response: %s", response.status_code, response.text)
                failed_profiles.append(profile)

        except requests.exceptions.RequestException as e:
            adapter.exception("Error occurred while pushing candidate profile. %s", e)
            failed_profiles.append(profile)

    return failed_profiles
   

# Define the Warehouse for Digital Recruiters jobs and profiles
DigitalRecruitersJobWarehouse = Warehouse(
    name="DigitalRecruiters Jobs",
    data_schema=DigitalRecruitersJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        endpoints=[ActionEndpoints(name="Read Jobs", description="Read jobs from Digital Recruiters", url=DIGITAL_RECRUITERS_JOBS_ENDPOINT)],
    ),
)

DigitalRecruitersProfileWarehouse = Warehouse(
    name="DigitalRecruiters Profiles",
    data_schema=DigitalRecruitersImportCandidateProfile,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[ActionEndpoints(name="Write Profiles", description="Write profiles to Digital Recruiters", url=DIGITAL_RECRUITERS_PROFILES_ENDPOINT)],
    ),
)
