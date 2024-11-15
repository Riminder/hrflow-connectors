import random
import string
import typing as t
from pathlib import Path
from unittest import mock

import pytest

from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import (
    Connector,
    ConnectorType,
    Flow,
    PublicActionInterface,
)
from tests.v2.core.src.hrflow_connectors.connectors.smartleads.aisles.candidates import (
    CANDIDATES_DB,
)
from tests.v2.core.src.hrflow_connectors.connectors.smartleads.aisles.leads import (
    LEADS_DB,
)
from tests.v2.core.src.hrflow_connectors.connectors.smartleads.warehouse import (
    SmartLeadsWarehouse,
)
from tests.v2.core.src.hrflow_connectors.core.hrflow_mini.aisles.applications import (
    APPLICATIONS_DB,
)
from tests.v2.core.src.hrflow_connectors.core.hrflow_mini.aisles.jobs import JOBS_DB
from tests.v2.core.src.hrflow_connectors.core.hrflow_mini.warehouse import (
    HrFlowMiniWarehouse,
)


def random_workflow_id() -> str:
    return "".join([random.choice(string.ascii_letters) for _ in range(10)])


@pytest.fixture
def connectors_directory() -> Path:
    return Path(__file__).parent / "src" / "hrflow_connectors" / "connectors"


@pytest.fixture(scope="function", autouse=True)
def reset_dbs():
    JOBS_DB.reset()
    LEADS_DB.reset()
    CANDIDATES_DB.reset()
    APPLICATIONS_DB.reset()

    yield


class SmartLeadsProto(t.Protocol):
    def __call__(
        self,
        name: t.Optional[str] = None,
        subtype: t.Optional[str] = None,
        description: t.Optional[str] = None,
        url: t.Optional[str] = None,
        type: t.Optional[ConnectorType] = None,
        flows: t.Optional[tuple[Flow, ...]] = None,
    ) -> Connector:
        ...


@pytest.fixture
def SmartLeadsF() -> t.Iterator[SmartLeadsProto]:
    with (
        mock.patch(
            "hrflow_connectors.v2.core.connector.HrFlowWarehouse",
            HrFlowMiniWarehouse,
        ),
        mock.patch(
            "hrflow_connectors.v2.core.templating.HrFlowWarehouse",
            HrFlowMiniWarehouse,
        ),
    ):

        def _SmartLeadsF(
            name: t.Optional[str] = None,
            subtype: t.Optional[str] = None,
            description: t.Optional[str] = None,
            url: t.Optional[str] = None,
            type: t.Optional[ConnectorType] = None,
            flows: t.Optional[tuple[Flow, ...]] = None,
        ):
            return Connector(
                name=name or "SmartLeads",
                subtype=subtype or "smartleads",
                description=description or "Welcome to SmartLeads",
                url=url or "https://smartleads.co",
                type=type or ConnectorType.ATS,
                warehouse=SmartLeadsWarehouse,
                flows=flows or tuple(),
            )

        yield _SmartLeadsF


class TypedSmartLeads(Connector):
    create_jobs_in_hrflow: PublicActionInterface
    update_jobs_in_hrflow: PublicActionInterface
    archive_jobs_in_hrflow: PublicActionInterface
    create_jobs_in_smartleads: PublicActionInterface
    update_jobs_in_smartleads: PublicActionInterface
    archive_jobs_in_smartleads: PublicActionInterface


def hrflow_job_to_smartleads_lead(hrflow: dict):
    lead = dict(
        id=sum([ord(char) for char in hrflow["key"]]),
        category="from_hrflow",
        designation=hrflow["name"],
        city=hrflow["location"]["city"],
        remote_allowed=hrflow["remote"],
    )
    return lead


def smartleads_lead_to_hrflow_job(lead: dict):
    hrflow = dict(
        key=str(lead["id"]),
        reference=f"smartleads::{lead['id']}",
        name=lead["designation"],
        location=dict(city=lead["city"]),
        remote=lead["remote_allowed"],
    )
    return hrflow


@pytest.fixture
def SmartLeads(SmartLeadsF: SmartLeadsProto) -> TypedSmartLeads:
    return t.cast(
        TypedSmartLeads,
        SmartLeadsF(
            flows=(
                Flow(
                    Mode.create,
                    Entity.job,
                    Direction.inbound,
                    format=smartleads_lead_to_hrflow_job,
                ),
                Flow(
                    Mode.update,
                    Entity.job,
                    Direction.inbound,
                    format=smartleads_lead_to_hrflow_job,
                ),
                Flow(
                    Mode.archive,
                    Entity.job,
                    Direction.inbound,
                    format=smartleads_lead_to_hrflow_job,
                ),
                Flow(
                    Mode.create,
                    Entity.job,
                    Direction.outbound,
                    format=hrflow_job_to_smartleads_lead,
                ),
                Flow(
                    Mode.update,
                    Entity.job,
                    Direction.outbound,
                    format=hrflow_job_to_smartleads_lead,
                ),
                Flow(
                    Mode.archive,
                    Entity.job,
                    Direction.outbound,
                    format=hrflow_job_to_smartleads_lead,
                ),
            )
        ),
    )
