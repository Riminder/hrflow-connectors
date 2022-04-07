import typing as t
from logging import LoggerAdapter

from hrflow import Hrflow
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.hrflow.schemas import HrFlowProfile
from hrflow_connectors.core import Warehouse, WarehouseReadAction


class ReadProfileParameters(BaseModel):
    api_secret: str = Field(
        ...,
        description="X-API-KEY used to access HrFlow.ai API",
        repr=False,
    )
    api_user: str = Field(..., description="X-USER-EMAIL used to access HrFlow.ai API")
    source_key: str = Field(..., description="HrFlow.ai source key")
    profile_key: str = Field(..., description="HrFlow.ai profile key")


def read(adapter: LoggerAdapter, parameters: ReadProfileParameters) -> t.List[t.Dict]:
    hrflow_client = Hrflow(
        api_secret=parameters.api_secret, api_user=parameters.api_user
    )
    response = hrflow_client.profile.indexing.get(
        source_key=parameters.source_key, key=parameters.profile_key
    )
    if "Unable to find object" in response["message"]:
        adapter.info(
            "No profile found for source_key={} profile_key={} response={}".format(
                parameters.source_key, parameters.profile_key, response
            )
        )
        return []
    elif response["code"] >= 400:
        adapter.error(
            "Failed to get profile source_key={} profile_key={} response={}".format(
                parameters.source_key, parameters.profile_key, response
            )
        )
        raise Exception("Failed to get profile")
    return [response["data"]]


HrFlowProfileWarehouse = Warehouse(
    name="HrFlow.ai Profiles",
    data_schema=HrFlowProfile,
    read=WarehouseReadAction(parameters=ReadProfileParameters, function=read),
)
