import hashlib
import hmac
import typing as t
from datetime import datetime
from enum import Enum
from logging import LoggerAdapter

import requests
from hrflow import Hrflow
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.hrflow.warehouse.job import (
    HrFlowJob,
    WriteJobParameters,
)
from hrflow_connectors.connectors.leboncoin.schemas import Ad
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

Leboncoin_ENDPOINT = "https://api.flux.qa.leboncoin.io/v2/ad/job"
HrFlow_JOBS_ENDPOINT = "https://api.hrflow.ai/v1/job/indexing"


class JobStatus(str, Enum):
    being_created = "BEING CREATED"
    cancelled = "CANCELLED"


class ReadJobParameters(ParametersModel):
    api_secret: str = Field(
        ...,
        description="X-API-KEY used to access HrFlow.ai API",
        repr=False,
        field_type=FieldType.Auth,
    )
    api_user: str = Field(
        ...,
        description="X-USER-EMAIL used to access HrFlow.ai API",
        field_type=FieldType.Auth,
    )
    board_key: str = Field(
        ...,
        description="HrFlow.ai board keys to extract the job from",
        field_type=FieldType.QueryParam,
    )
    job_key: str = Field(..., description="", field_type=FieldType.QueryParam)


class WriteAdsParameters(ParametersModel):
    secret_key: str = Field(
        ...,
        description="Leboncoin API secret key",
        repr=False,
        field_type=FieldType.Auth,
    )
    http_auth_identity: str = Field(
        ..., description="Client's name", field_type=FieldType.Auth
    )
    morpheus_client_id: int = Field(
        ..., description="Client's unique ID", field_type=FieldType.Auth
    )


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.List[t.Dict]:
    hrflow_client = Hrflow(
        api_secret=parameters["api_secret"], api_user=parameters["api_user"]
    )
    response = hrflow_client.job.indexing.get(
        board_key=parameters["board_key"], key=parameters["job_key"]
    )
    if "Unable to find object" in response["message"]:
        adapter.info(
            "No job found for board_key={} job_key={} response={}".format(
                parameters.board_key, parameters.job_key, response
            )
        )
        return []
    elif response["code"] >= 400:
        adapter.error(
            "Failed to get job board_key={} job_key={} response={}".format(
                parameters.source_key, parameters.profile_key, response
            )
        )
        raise Exception("Failed to get profile")
    return [response["data"]]


def write(
    adapter: LoggerAdapter, parameters: WriteAdsParameters, ads: t.Iterable[t.Dict]
) -> t.List[t.Dict]:
    failed_items = []
    ads = list(ads)
    for ad in ads:
        http_auth_date = datetime.utcnow().strftime("%Y-%m-%d %H:%M:%S")
        http_auth_mac = hmac.new(
            bytes(parameters.secret_key.encode()),
            msg=http_auth_date.encode(),
            digestmod=hashlib.sha256,
        ).hexdigest()
        response = requests.post(
            Leboncoin_ENDPOINT,
            json=ad,
            headers={
                "HTTP_AUTH_HMAC": http_auth_mac,
                "HTTP_AUTH_DATE": http_auth_date,
                "HTTP_AUTH_IDENTITY": parameters.http_auth_identity,
                "Content_Type": "application/json",
            },
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create ad status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_items.append(ad)
    return failed_items


LeboncoinWarehouse = Warehouse(
    name="LeboncoinWarehouse",
    data_schema=Ad,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobParameters,
        function=read,
    ),
    write=WarehouseWriteAction(
        parameters=WriteAdsParameters,
        function=write,
    ),
)
HrFlowJobWarehouse = Warehouse(
    name="HrFlow.ai Jobs",
    data_schema=HrFlowJob,
    data_type=DataType.job,
    read=WarehouseReadAction(parameters=ReadJobParameters, function=read),
    write=WarehouseWriteAction(parameters=WriteJobParameters, function=write),
)
