import enum
import hashlib
import hmac
import time
import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field
from hrflow_connectors.connectors.broadbean.schemas import BroadbeanCandidate

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

BROADBEAN_PUSH_ENDPOINT = "https://candidatehub.adcourier.com/candidate" #this endpoint is to be transmitted to bradbean
BROADBEAN_PULL_ENDPOINT = "https://url_of_your_candidate_importer.com/importer_route"

CATCH_CANDIDATE_ENDPOINT = ActionEndpoints(
    name="Get candidate",
    description=(
        "This will be a route on the receiving application or " 
        "integration and not the candidate hub. This route will accept standard " 
        "candidate data and return a standard response to be consumed by the hub. "
        "If parsing is enabled, two additional candidate documents will be included. " 
        "The raw parsed XML as a tagged_cv and a JSON document of type parsed_cv " 
        "(both Base64 encoded)."

    ),
    url=(
        "https://integrations.broadbean.com/hc/en-us/articles/115004599649-Receiving-a-Candidate"
    ),
)
POST_CANDIDATE_ENDPOINT = ActionEndpoints(
    name="Send candidate",
    description=(
        "This route accepts a POST request consisting of a JSON payload "
        "and some authentication Headers. The response will consist of a "
        "success flag and either a transaction ID (success) or an error." 
    ),
    url=(
        "https://integrations.broadbean.com/hc/en-us/articles/115004599865-Sending-a-Candidate-in-"
    ),
)


class WriteProfilesParameters(ParametersModel):
    secret_key: str = Field(
        ...,
        description="secret provided by Broadbean",
        repr=False,
        field_type = FieldType.Auth,
    )
    source_id: str = Field(
        ...,
        description = "Candidate Hub source ID",
        field_type = FieldType.Auth
    )
    job_id : str = Field(
        ...,
        description = "The ID of the Job the candidate has applied to",
        field_type = FieldType.QueryParam
    )


class ReadCandidateParameters(ParametersModel):
    secret_key: str = Field(
        ...,
        description="secret provided by Broadbean",
        repr=False,
        field_type = FieldType.Auth,
    )
    api_key: str = Field(
        ...,
        description=("The API Key used to generate the signature. "
        "This is provided by Broadbean at the time of setup"),
        repr=False,
        field_type=FieldType.Auth,
    )
    profile: t.Optional[t.Dict] = Field(
        None,
        description="Optional profile for testing",
        field_type=FieldType.QueryParam,
    )

def catch(
    adapter: LoggerAdapter,
    parameters: ReadCandidateParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    return parameters.profile



def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info(
        "Pushing {} profiles with job_id={}".format(len(profiles), parameters.job_id)
    )
    failed_profiles = []
    for profile in profiles:
        profile["context"] = {"job_id": parameters.job_id}
        
        #Authentication
        epoch_time = int(time.time())
        transaction_id = parameters.source_id + str(epoch_time)
        signature = hmac.new(parameters.secret_key.encode('utf-8'), transaction_id.encode('utf-8'), hashlib.sha256).hexdigest()

        response = requests.post(
            BROADBEAN_PUSH_ENDPOINT,
            headers={
                "X-X-CHUB-SIGNATURE-TIME": str(epoch_time),
                "X-CHUB-SOURCE": parameters.source_id,
                "X-CHUB-SIGNATURE": signature,
            },
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Broadbean job_id={}"
                " status_code={} response={}".format(
                    parameters.job_id,
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


BroadbeanCandidateWarehouse = Warehouse(
    name="SmartRecruiters Jobs",
    data_schema=BroadbeanCandidate,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadCandidateParameters,
        function=catch,
        endpoints=[CATCH_CANDIDATE_ENDPOINT],
    ),
)

BroadbeanProfileWarehouse = Warehouse(
    name="SmartRecruiters Profiles",
    data_schema=BroadbeanCandidate,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[POST_CANDIDATE_ENDPOINT],
    ),
)
