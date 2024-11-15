import random
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

from ..schemas import Lead
from .common import SECRET_SMART_TAG, AuthParameters


def get_id():
    return random.randint(1, 99999)


LEADS = [
    dict(
        id=1,
        category="IT",
        designation="Software Engineer",
        status="created",
        city="Casablanca",
        remote_allowed=True,
    ),
    dict(
        id=2,
        category="Heavy Works",
        designation="Mechanical Engineer",
        status="updated",
        city="Rabat",
        remote_allowed=False,
    ),
    dict(
        id=3,
        category="Services",
        designation="Barber",
        status="updated",
        city="Casablanca",
        remote_allowed=False,
    ),
    dict(
        id=4,
        category="Services",
        designation="Truck Driver",
        status="archived",
        city="Tanger",
        remote_allowed=False,
    ),
    dict(
        id=5,
        category="Healthcare",
        designation="Doctor",
        status="created",
        city="Azrou",
        remote_allowed=False,
    ),
    dict(
        id=6,
        category="Argiculture",
        designation="Farmer",
        status="updated",
        city="Agadir",
        remote_allowed=False,
    ),
    dict(
        id=7,
        category="Corporate",
        designation="Scrum Master",
        status="updated",
        city="Meknes",
        remote_allowed=True,
    ),
    dict(
        id=8,
        category="Corporate",
        designation="Data Analyst",
        status="archived",
        city="Fes",
        remote_allowed=True,
    ),
]
LEADS_DB = DB(LEADS)


class CreateCriterias(Struct):
    force_candidate_count_zero: t.Optional[bool] = False


class UpdateCriterias(Struct):
    pass


class ArchiveCriterias(Struct):
    reset_candidate_count: t.Optional[bool] = True


def create(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: CreateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    adapter.info("Starting create operation")
    if auth_parameters.smart_tag != SECRET_SMART_TAG:
        adapter.error("Wrong Smart Tag !!")
        raise Exception("Wrong Smart Tag")

    failed_leads = []
    for lead in items:
        lead.setdefault("id", get_id())
        new_lead = {**lead, "status": "created"}
        try:
            convert(new_lead, Lead)
        except ValidationError:
            failed_leads.append(lead)
            continue
        if parameters.force_candidate_count_zero:
            lead["candidate_count"] = 0
        LEADS_DB.append(new_lead)

    adapter.info("Finished create operation")
    return failed_leads


def update(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    adapter.info("Starting update operation")
    if auth_parameters.smart_tag != SECRET_SMART_TAG:
        adapter.error("Wrong Smart Tag !!")
        raise Exception("Wrong Smart Tag")

    failed_leads = []
    for lead in items:
        index, lead_to_update = next(
            (
                (index, _lead)
                for index, _lead in enumerate(LEADS_DB)
                if _lead["id"] == lead["id"]
            ),
            (None, None),
        )
        if lead_to_update is None or index is None:
            continue

        updated = {**lead_to_update, **lead, "status": "updated"}
        try:
            convert(updated, Lead)
        except ValidationError:
            failed_leads.append(lead)
            continue
        LEADS_DB[index] = updated

    adapter.info("Finished update operation")
    return failed_leads


def archive(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ArchiveCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    adapter.info("Starting archive operation")
    if auth_parameters.smart_tag != SECRET_SMART_TAG:
        adapter.error("Wrong Smart Tag !!")
        raise Exception("Wrong Smart Tag")

    for lead in items:
        index, lead_to_archive = next(
            (
                (index, _lead)
                for index, _lead in enumerate(LEADS_DB)
                if _lead["id"] == lead["id"]
            ),
            (None, None),
        )
        if lead_to_archive is None or index is None:
            continue

        LEADS_DB[index]["status"] = "archived"
        if parameters.reset_candidate_count:
            LEADS_DB[index]["candidate_count"] = 0

    adapter.info("Finished archive operation")
    return []


class ReadCreatedCriterias(Struct):
    city: t.Optional[str] = None


class ReadUpdatedCriterias(Struct):
    category: t.Optional[str] = None


class ReadArchivedCriterias(Struct):
    remote: Annotated[t.Optional[bool], Meta(description="Only remote leads")] = None


def read_created(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadCreatedCriterias,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> list[dict]:
    adapter.info("Reading created")
    if auth_parameters.smart_tag != SECRET_SMART_TAG:
        adapter.error("Wrong Smart Tag !!")
        raise Exception("Wrong Smart Tag")

    if incremental:
        created = [
            lead
            for lead in LEADS_DB
            if lead["status"] == "created"
            and t.cast(int, lead["id"]) > int(incremental_token or "0")
        ]
    else:
        created = [lead for lead in LEADS_DB if lead["status"] == "created"]

    if parameters.city is not None:
        created = [lead for lead in created if lead["city"] == parameters.city]

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
    if auth_parameters.smart_tag != SECRET_SMART_TAG:
        adapter.error("Wrong Smart Tag !!")
        raise Exception("Wrong Smart Tag")

    if incremental:
        updated = [
            lead
            for lead in LEADS_DB
            if lead["status"] == "updated"
            and t.cast(int, lead["id"]) >= int(incremental_token or "0")
        ]
    else:
        updated = [lead for lead in LEADS_DB if lead["status"] == "updated"]

    if parameters.category is not None:
        updated = [lead for lead in updated if lead["category"] == parameters.category]

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
    if auth_parameters.smart_tag != SECRET_SMART_TAG:
        adapter.error("Wrong Smart Tag !!")
        raise Exception("Wrong Smart Tag")

    if incremental:
        archived = [
            lead
            for lead in LEADS_DB
            if lead["status"] == "archived"
            and t.cast(int, lead["id"]) >= int(incremental_token or "0")
        ]
    else:
        archived = [lead for lead in LEADS_DB if lead["status"] == "archived"]

    if parameters.remote is not None:
        archived = [
            lead for lead in archived if lead["remote_allowed"] == parameters.remote
        ]

    adapter.info("Finished reading archived")
    return archived


LeadsAisle = Aisle(
    name=Entity.job,
    read=ReadOperation(
        function=merge(create=read_created, update=read_updated, archive=read_archived),
        criterias=Criterias(
            create=ReadCreatedCriterias,
            update=ReadUpdatedCriterias,
            archive=ReadArchivedCriterias,
        ),
        get_incremental_token=lambda lead: str(lead["id"]),
    ),
    write=WriteOperation(
        function=merge(create=create, update=update, archive=archive),
        criterias=Criterias(
            create=CreateCriterias, update=UpdateCriterias, archive=ArchiveCriterias
        ),
    ),
    schema=Lead,
)
