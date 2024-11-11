import typing as t
from logging import LoggerAdapter

from msgspec import Meta, Struct, ValidationError, convert
from typing_extensions import Annotated

from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)
from tests.v2.utils import DB

from ..schemas import HrFlowMiniJob
from .common import SECRET_API_KEY, AuthParameters

JOBS = [
    dict(
        key="0001",
        board_key="xxxyyy",
        reference="ref-0001",
        name="Software Engineer",
        status="created",
        location=dict(city="Casablanca"),
        remote=True,
    ),
    dict(
        key="0002",
        board_key="zzzuuu",
        reference="ref-0002",
        name="Mechanical Engineer",
        status="updated",
        location=dict(city="Rabat"),
        remote=False,
    ),
    dict(
        key="0003",
        board_key="zzzuuu",
        reference="ref-0003",
        name="Barber",
        status="updated",
        location=dict(city="Casablanca"),
        remote=False,
    ),
    dict(
        key="0004",
        board_key="zzzuuu",
        reference="ref-0004",
        name="Truck Driver",
        status="archived",
        location=dict(city="Tanger"),
        remote=False,
    ),
    dict(
        key="0005",
        board_key="xxxyyy",
        reference="ref-0005",
        name="Doctor",
        status="created",
        location=dict(city="Azrou"),
        remote=False,
    ),
    dict(
        key="0006",
        board_key="zzzuuu",
        reference="ref-0006",
        name="Farmer",
        status="updated",
        location=dict(city="Agadir"),
        remote=False,
    ),
    dict(
        key="0007",
        board_key="xxxyyy",
        reference="ref-0007",
        name="Scrum Master",
        status="updated",
        location=dict(city="Meknes"),
        remote=True,
    ),
    dict(
        key="0008",
        board_key="zzzuuu",
        reference="ref-0008",
        name="Scrum Master",
        status="archived",
        location=dict(city="Salé"),
        remote=True,
    ),
]
JOBS_DB = DB(JOBS)


class CreateCriterias(Struct):
    board_key: Annotated[str, Meta(description="HrFlow.ai board key")]


class UpdateCriterias(Struct):
    pass


class ArchiveCriterias(Struct):
    board_key: Annotated[str, Meta(description="HrFlow.ai board key")]


def create(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: CreateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    adapter.info("Starting create operation")
    if auth_parameters.api_key != SECRET_API_KEY:
        adapter.error("Bad credentials !!")
        raise Exception("Bad credentials")

    failed_jobs = []
    for job in items:
        new_job = {**job, "status": "created", "board_key": parameters.board_key}
        try:
            convert(new_job, HrFlowMiniJob)
        except ValidationError:
            failed_jobs.append({**job})
            continue
        JOBS_DB.append(new_job)

    adapter.info("Finished create operation")
    return failed_jobs


def update(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    adapter.info("Starting update operation")
    if auth_parameters.api_key != SECRET_API_KEY:
        adapter.error("Bad credentials !!")
        raise Exception("Bad credentials")

    failed_jobs = []
    for job in items:
        index, job_to_update = next(
            (
                (index, _job)
                for index, _job in enumerate(JOBS_DB)
                if _job["key"] == job["key"]
            ),
            (None, None),
        )
        if job_to_update is None or index is None:
            continue

        updated = {**job_to_update, **job, "status": "updated"}
        try:
            convert(updated, HrFlowMiniJob)
        except ValidationError:
            failed_jobs.append(job)
            continue
        JOBS_DB[index] = updated

    adapter.info("Finished update operation")
    return failed_jobs


def archive(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ArchiveCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    adapter.info("Starting archive operation")
    if auth_parameters.api_key != SECRET_API_KEY:
        adapter.error("Bad credentials !!")
        raise Exception("Bad credentials")

    for job in items:
        index, job_to_archive = next(
            (
                (index, _job)
                for index, _job in enumerate(JOBS_DB)
                if _job["key"] == job["key"]
                and _job["board_key"] == parameters.board_key
            ),
            (None, None),
        )
        if job_to_archive is None or index is None:
            continue

        JOBS_DB[index]["status"] = "archived"

    adapter.info("Finished archive operation")
    return []


class ReadCreatedCriterias(Struct):
    city: t.Optional[str] = None


class ReadUpdatedCriterias(Struct):
    city: t.Optional[str] = None
    remote: Annotated[t.Optional[bool], Meta(description="Only remote jobs")] = None


class ReadArchivedCriterias(Struct):
    remote: Annotated[t.Optional[bool], Meta(description="Only remote jobs")] = None


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

    created = [job for job in JOBS_DB if job["status"] == "created"]

    if parameters.city is not None:
        created = [
            job
            for job in created
            if t.cast(dict, job["location"])["city"] == parameters.city
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

    updated = [job for job in JOBS_DB if job["status"] == "updated"]

    if parameters.city is not None:
        updated = [
            job
            for job in updated
            if t.cast(dict, job["location"])["city"] == parameters.city
        ]

    if parameters.remote is not None:
        updated = [job for job in updated if job["remote"] == parameters.remote]

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

    archived = [job for job in JOBS_DB if job["status"] == "archived"]

    if parameters.remote is not None:
        archived = [job for job in archived if job["remote"] == parameters.remote]

    adapter.info("Finished reading archived")
    return archived


JobsAisle = Aisle(
    name=Entity.job,
    read=ReadOperation(
        function=merge(create=read_created, update=read_updated, archive=read_archived),
        criterias=Criterias(
            create=ReadCreatedCriterias,
            update=ReadUpdatedCriterias,
            archive=ReadArchivedCriterias,
        ),
    ),
    write=WriteOperation(
        function=merge(create=create, update=update, archive=archive),
        criterias=Criterias(
            create=CreateCriterias, update=UpdateCriterias, archive=ArchiveCriterias
        ),
    ),
    schema=HrFlowMiniJob,
)
