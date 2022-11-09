import typing as t
import xml.etree.ElementTree as ET
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.hrflow.schemas import HrFlowJob
from hrflow_connectors.connectors.monster.schemas import MonsterProfile
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

PUSH_JOB_ENDPOINT = ActionEndpoints(
    name="Get job",
    description=(
        "Endpoint to push the content of a job with a given job key and board key"
        " the request method is `POST`"
    ),
    url="https://partner.monster.com/real-time-posting-devguide",
)
GET_PROFILE_ENDPOINT = ActionEndpoints(
    name="Post Candidate",
    description="Endpoint to catch a profile and assign it to a source in hrflow",
    url="https://partner.monster.com/apply-with-monster-implementing",
)


class PushJobsParameters(ParametersModel):
    username: str = Field(
        ..., description="Monster username", field_type=FieldType.Auth
    )
    password: str = Field(
        ..., description="Monster password", field_type=FieldType.Auth
    )
    api_key: str = Field(
        ..., description="API key to submit", field_type=FieldType.Auth
    )
    subdomain: str = Field(
        ...,
        description=(
            "Subdomain monster just before `monster.com`. For example"
            " subdomain=`my_subdomain.my` in"
            " `https//my_subdomain.my.monster.com8443/bgwBroker"
        ),
        field_type=FieldType.QueryParam,
    )


class CatchProfilesParameters(ParametersModel):
    profile: t.Optional[t.Dict] = Field(
        None,
        description="Optional profile for testing",
        field_type=FieldType.QueryParam,
    )


def catch_profile(
    adapter: LoggerAdapter,
    parameters: CatchProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Dict:
    return [parameters.profile]


namespace = {
    "xmlns": "http://schemas.xmlsoap.org/soap/envelope/",
    "mh": "http://schemas.monster.com/MonsterHeader",
    "wsse": "http://schemas.xmlsoap.org/ws/2002/04/secext",
    "xsi": "http://schemas.monster.com/Monster",
}


def push_job(
    adapter: LoggerAdapter,
    parameters: PushJobsParameters,
    jobs: t.List[str],
) -> t.List[str]:
    failed_jobs = []
    for job in jobs:
        # add the authentification fields to the job xml
        job_xml = ET.fromstring(job)
        # username field
        job_xml.find("xmlns:Header", namespace).find("wsse:Security", namespace).find(
            "wsse:UsernameToken", namespace
        ).find("wsse:Username", namespace).text = parameters.username
        # password field
        job_xml.find("xmlns:Header", namespace).find("wsse:Security", namespace).find(
            "wsse:UsernameToken", namespace
        ).find("wsse:Password", namespace).text = parameters.password
        # recruiter_username field
        job_xml.find("xmlns:Body", namespace).find("xsi:Job", namespace).find(
            "xsi:RecruiterReference", namespace
        ).find("xsi:UserName", namespace).text = parameters.username
        # apikey field
        job_xml.find("xmlns:Body", namespace).find("xsi:Job", namespace).find(
            "xsi:JobInformation", namespace
        ).find("xsi:ApplyWithMonster", namespace).find(
            "xsi:ApiKey", namespace
        ).text = parameters.api_key
        job = ET.tostring(job_xml, encoding="utf8", method="xml")

        response = requests.post(
            "https://{}.monster.com:8443/bgwBroker".format(parameters.subdomain),
            headers={"content-type": "text/xml"},
            data=job,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Monster  status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
    return failed_jobs


MonsterJobWarehouse = Warehouse(
    name="Monster Jobs",
    data_schema=HrFlowJob,
    data_type=DataType.job,
    write=WarehouseWriteAction(
        parameters=PushJobsParameters,
        function=push_job,
        endpoints=[PUSH_JOB_ENDPOINT],
    ),
)

MonsterProfileWarehouse = Warehouse(
    name="Monster Profiles",
    data_schema=MonsterProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=CatchProfilesParameters,
        function=catch_profile,
        endpoints=[GET_PROFILE_ENDPOINT],
    ),
)
