import typing as t
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


class WriteProfilesParameters(ParametersModel):
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
        description="Passoword for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )

    username: str = Field(
        ...,
        description="Username for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadJobsParameters(ParametersModel):
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
        description="Passoword for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )

    username: str = Field(
        ...,
        description="Username for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadProfileParameters(ParametersModel):
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
        description="Passoword for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )

    username: str = Field(
        ...,
        description="Username for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )


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
            print("ed")
            education = education
            education["candidate"]["id"] = candidate_id
            education_url = rest_url + "entity/CandidateEducation"
            response = requests.put(url=education_url, json=education, params=params)

        # Enrich profile experience
        for experience in enrich_profile_experience:
            print("exp")
            experience["candidate"]["id"] = candidate_id
            experience_url = rest_url + "entity/CandidateWorkHistory"
            response = requests.put(url=experience_url, json=experience, params=params)

        # Enrich profile attachements
        for attachment in enrich_profile_attachment:
            print("att")
            attachment_url = rest_url + "file/Candidate/" + str(candidate_id)
            response = requests.put(url=attachment_url, json=attachment, params=params)

    return failed_profiles


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
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

        jobs_url = (
            authentication["restUrl"]
            + "search/JobOrder?query=(isOpen:true)&fields=*&BhRestToken="
            + authentication["BhRestToken"]
            + "&start="
            + str(start)
        )

        response = requests.get(url=jobs_url)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Bullhorn status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from Bullhorn")
        response = response.json()

        start = response["start"]
        data = response["data"]

        for job in data:
            yield job

        if (response["count"] + start) >= response["count"]:
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
    start = 0
    print("hello")
    while True:

        profiles_url = (
            authentication["restUrl"] + "myCandidates?fields=*&start=" + str(start)
        )
        print(profiles_url)
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

        start = response["start"]
        data = response["data"]

        for profile in data:
            yield profile

        if (response["count"] + start) >= response["count"]:
            break


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

BullhornJobWarehouse = Warehouse(
    name="Bullhorn Jobs",
    data_schema=BullhornJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        endpoints=[],
    ),
)
