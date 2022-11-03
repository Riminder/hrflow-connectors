import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.bullhorn.schemas import BullhornProfile
from hrflow_connectors.connectors.bullhorn.utils.authentication import auth
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseWriteAction,
)


class WriteProfilesParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="",
        repr=False,
        field_type=FieldType.Auth,
    )

    client_secret: str = Field(
        ...,
        description="",
        repr=False,
        field_type=FieldType.Auth,
    )

    password: str = Field(
        ...,
        description="",
        repr=False,
        field_type=FieldType.Auth,
    )

    username: str = Field(
        ...,
        description="",
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
        if response.status_code // 100 != 2:
            failed_profiles.append(profile)
            continue

        candidate_id = response.json()
        candidate_id = str(candidate_id["changedEntityId"])

        for education in enrich_profile_education:
            print("ed")
            education = education
            education["candidate"]["id"] = candidate_id
            education_url = rest_url + "entity/CandidateEducation"
            response = requests.put(url=education_url, json=education, params=params)

        for experience in enrich_profile_experience:
            print("exp")
            experience["candidate"]["id"] = candidate_id
            experience_url = rest_url + "entity/CandidateWorkHistory"
            response = requests.put(url=experience_url, json=experience, params=params)

        for attachment in enrich_profile_attachment:
            print("att")
            attachment_url = rest_url + "file/Candidate/" + str(candidate_id)
            response = requests.put(url=attachment_url, json=attachment, params=params)

    return failed_profiles


BullhornProfileWarehouse = Warehouse(
    name="Bullhorn Profiles",
    data_schema=BullhornProfile,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[],
    ),
)
