import typing as t
from collections import defaultdict
from logging import LoggerAdapter

from pydantic import BaseModel, ConstrainedStr

from hrflow_connectors.core import ActionEndpoints, Warehouse, WarehousePushAction

POST_LEADS = ActionEndpoints(
    name="Post lead",
    description="Post Lead on specific campaign_id",
    url="https://local_leads.readthedocs.com/leads",
)


LEADS_DB = defaultdict(list)


class Lead(BaseModel):
    name: str
    email: str


class PushLeadsParameters(BaseModel):
    campaign_id: str
    dummy_int: t.Optional[int]
    dummy_str: str = "xxx"
    dummy_const_str: t.Optional[ConstrainedStr]


def push(
    adapter: LoggerAdapter,
    parameters: PushLeadsParameters,
    items: t.Iterator[t.Dict],
) -> None:
    adapter.info("Pushing leads to DB")
    LEADS_DB.setdefault(parameters.campaign_id, []).extend(items)
    adapter.info("Finished pushing leads to DB")


LeadsWarehouse = Warehouse(
    name="Test Leads",
    data_schema=Lead,
    push=WarehousePushAction(
        parameters=PushLeadsParameters,
        function=push,
        endpoints=[POST_LEADS],
    ),
)

BadLeadsWarehouse = Warehouse(
    name="Bad Test Leads",
    data_schema=Lead,
    push=WarehousePushAction(
        parameters=PushLeadsParameters,
        function=lambda *args, **kwargs: 10 / 0,
        endpoints=[POST_LEADS],
    ),
)
