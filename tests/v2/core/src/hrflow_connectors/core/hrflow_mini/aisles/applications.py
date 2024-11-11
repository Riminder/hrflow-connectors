import typing as t
from logging import LoggerAdapter

from msgspec import Struct

from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    merge,
)
from tests.v2.utils import DB

from ..schemas import HrFlowMiniApplication
from .common import SECRET_API_KEY, AuthParameters

APPLICATIONS = [
    dict(job_key="0001", candidate_id=4454, status="created", outcome="pending"),
    dict(job_key="0002", candidate_id=5554, status="updated", outcome="accepted"),
    dict(job_key="0003", candidate_id=85567784, status="updated", outcome="accepted"),
    dict(job_key="0004", candidate_id=677875, status="archived", outcome="rejected"),
    dict(job_key="0005", candidate_id=54322, status="created", outcome="pending"),
    dict(job_key="0006", candidate_id=985, status="updated", outcome="rejected"),
    dict(job_key="0007", candidate_id=3295357, status="updated", outcome="pending"),
]
APPLICATIONS_DB = DB(APPLICATIONS)


class ReadCreatedCriterias(Struct):
    pass


class ReadUpdatedCriterias(Struct):
    pass


class ReadArchivedCriterias(Struct):
    pass


def read_created(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadCreatedCriterias,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> list[dict]:
    adapter.info("Reading created")
    if auth_parameters.api_key != SECRET_API_KEY:
        adapter.error("Bad credentials !!")
        raise Exception("Bad credentials")

    created = [
        application
        for application in APPLICATIONS_DB
        if application["status"] == "created"
    ]

    adapter.info("Finished reading created")
    return created


def read_updated(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadUpdatedCriterias,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> list[dict]:
    adapter.info("Reading updated")
    if auth_parameters.api_key != SECRET_API_KEY:
        adapter.error("Bad credentials !!")
        raise Exception("Bad credentials")

    updated = [
        application
        for application in APPLICATIONS_DB
        if application["status"] == "updated"
    ]

    adapter.info("Finished reading updated")
    return updated


def read_archived(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadArchivedCriterias,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> list[dict]:
    adapter.info("Reading archived")
    if auth_parameters.api_key != SECRET_API_KEY:
        adapter.error("Bad credentials !!")
        raise Exception("Bad credentials")

    archived = [
        application
        for application in APPLICATIONS_DB
        if application["status"] == "archived"
    ]

    adapter.info("Finished reading archived")
    return archived


ApplicationsAisle = Aisle(
    name=Entity.application,
    read=ReadOperation(
        function=merge(create=read_created, update=read_updated, archive=read_archived),
        criterias=Criterias(
            create=ReadCreatedCriterias,
            update=ReadUpdatedCriterias,
            archive=ReadArchivedCriterias,
        ),
    ),
    schema=HrFlowMiniApplication,
)
