import typing as t
from logging import LoggerAdapter

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.connectors.workable.schemas import WorkableJobModel
from hrflow_connectors.core import DataType, ReadMode, Warehouse, WarehouseReadAction
from hrflow_connectors.core.warehouse import WarehouseWriteAction


class WorkableReadParameters(BaseModel):
    auth: str = Field(..., description="API KEY")
    subdomain: str = Field(..., description="Subdomain")


def read(
    adapter: LoggerAdapter,
    parameters: WorkableReadParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterator[WorkableJobModel]:

    url = f"https://{parameters.subdomain}.workable.com/spi/v3/jobs?state=published"

    payload = {}
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {parameters.auth}",
    }

    response = requests.get(url, headers=headers, data=payload)

    if not response.ok:
        adapter.error(f"Fail to read Workable jobs reason : {response.content}")

    for job in response.json().get("jobs"):
        yield job


class WorkableWriteParameters(BaseModel):
    auth: str = Field(..., description="API KEY")
    subdomain: str = Field(..., description="Subdomain")
    shortcode: str = Field(..., description="Job shortcode")


def write(
    adapter: LoggerAdapter,
    parameters: WorkableWriteParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:

    failed_profiles = []

    for profile in profiles:
        url = (
            f"https://{parameters.subdomain}.workable.com/"
            f"spi/v3/jobs/{parameters.shortcode}/candidates"
        )

        payload = profile

        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {parameters.auth}",
            "Accept": "application/json",
        }

        response = requests.post(url, headers=headers, data=payload)
        if not response.ok:
            failed_profiles.append(profile)

    return failed_profiles


WorkableJobWarehouse = Warehouse(
    name="WorkableJobWarehouse",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=WorkableReadParameters,
        function=read,
    ),
)

WorkableProfileWarehouse = Warehouse(
    name="WorkableProfileWarehouse",
    data_type=DataType.job,
    write=WarehouseWriteAction(
        parameters=WorkableWriteParameters,
        function=write,
    ),
)
