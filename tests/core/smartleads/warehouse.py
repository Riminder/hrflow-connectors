import typing as t
from collections import defaultdict
from logging import LoggerAdapter

from pydantic import BaseModel, ConstrainedStr

from hrflow_connectors.core import (
    ActionEndpoints,
    DataType,
    Warehouse,
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


class WriteLeadsParameters(BaseModel):
    campaign_id: str
    dummy_int: t.Optional[int]
    dummy_str: str = "xxx"
    dummy_const_str: t.Optional[ConstrainedStr]


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
    data_schema=Lead,
    data_type=DataType.other,
    write=WarehouseWriteAction(
        parameters=WriteLeadsParameters,
        function=write,
        endpoints=[POST_LEADS],
    ),
)


FailingLeadsWarehouse = Warehouse(
    name="Test Leads",
    data_schema=Lead,
    data_type=DataType.other,
    write=WarehouseWriteAction(
        parameters=WriteLeadsParameters,
        function=write_with_failures,
        endpoints=[POST_LEADS],
    ),
)

BadLeadsWarehouse = Warehouse(
    name="Bad Test Leads",
    data_schema=Lead,
    data_type=DataType.other,
    write=WarehouseWriteAction(
        parameters=WriteLeadsParameters,
        function=lambda *args, **kwargs: 10 / 0,
        endpoints=[POST_LEADS],
    ),
)
