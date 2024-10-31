import typing as t
from collections import defaultdict
from logging import LoggerAdapter

from pydantic import BaseModel, ConstrainedStr, Field

from hrflow_connectors.core.warehouse_v2 import (
    ActionEndpoints,
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseType,
    WarehouseWriteAction,
)

POST_LEADS = ActionEndpoints(
    name="Post lead",
    description="Post Lead on specific campaign_id",
    url="https://local_leads.readthedocs.com/leads",
)


LEADS_DB = defaultdict(list)


class Lead(BaseModel):
    name: str
    email: str


class AuthParameters(ParametersModel):
    api_secret: str = Field(None, repr=False, field_type=FieldType.Auth)
    api_user: str = Field(None, field_type=FieldType.Auth)


class WriteLeadsParameters(ParametersModel):
    campaign_id: str = Field(..., field_type=FieldType.Other)
    dummy_int: t.Optional[int] = Field(None, field_type=FieldType.Other)
    dummy_str: str = Field("xxx", field_type=FieldType.Other)
    dummy_const_str: t.Optional[ConstrainedStr] = Field(
        None, field_type=FieldType.Other
    )
    dummy_any: t.Optional[t.Any] = Field(None, field_type=FieldType.Other)


def write(
    adapter: LoggerAdapter,
    parameters: WriteLeadsParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing leads to DB")
    LEADS_DB.setdefault(parameters.campaign_id, []).extend(items)
    adapter.info("Finished writing leads to DB")
    return []


def write_with_failures(
    adapter: LoggerAdapter,
    parameters: WriteLeadsParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing leads to DB")
    items = list(items)
    LEADS_DB.setdefault(parameters.campaign_id, []).extend(items[:-2])
    adapter.info("Finished writing leads to DB")
    return items[-2:]


LeadsWarehouse = Warehouse(
    name="Test Leads",
    type=WarehouseType.inbound,
    data_schema=Lead,
    data_type=DataType.other,
    create=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=WriteLeadsParameters,
        function=write,
        endpoints=[POST_LEADS],
    ),
)


FailingLeadsWarehouse = Warehouse(
    name="Test Leads",
    type=WarehouseType.inbound,
    data_schema=Lead,
    data_type=DataType.other,
    create=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=WriteLeadsParameters,
        function=write_with_failures,
        endpoints=[POST_LEADS],
    ),
)

BadLeadsWarehouse = Warehouse(
    name="Bad Test Leads",
    type=WarehouseType.inbound,
    data_schema=Lead,
    data_type=DataType.other,
    create=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=WriteLeadsParameters,
        function=lambda *args, **kwargs: 10 / 0,
        endpoints=[POST_LEADS],
    ),
)
