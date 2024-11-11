import random
import typing as t
from logging import LoggerAdapter

from msgspec import Struct, ValidationError, convert

from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    WriteOperation,
    merge,
)
from tests.v2.utils import DB

from ..schemas import Candidate
from .common import SECRET_SMART_TAG, AuthParameters


def get_id():
    return random.randint(1, 99999)


CANDIDATES = [
    dict(
        id=12454,
        first_name="Omar",
        last_name="Darbour",
        status="created",
        age=45,
        has_driving_license=True,
    ),
    dict(
        id=5444,
        first_name="Said",
        last_name="Manda",
        status="created",
        age=24,
        has_driving_license=False,
    ),
    dict(
        id=5543,
        first_name="Majid",
        last_name="Bassam",
        status="archived",
        age=30,
        has_driving_license=True,
    ),
    dict(
        id=23567,
        first_name="Farid",
        last_name="Touiji",
        status="updated",
        age=32,
        has_driving_license=True,
    ),
    dict(
        id=3478,
        first_name="Nabil",
        last_name="Nimmi",
        status="updated",
        age=20,
        has_driving_license=True,
    ),
    dict(
        id=4467,
        first_name="Samsan",
        status="updated",
        last_name="Khan",
        age=29,
        has_driving_license=False,
    ),
    dict(
        id=3468,
        first_name="Malik",
        status="archived",
        last_name="Fawaz",
        age=36,
        has_driving_license=True,
    ),
]
CANDIDATES_DB = DB(CANDIDATES)


class CreateCriterias(Struct):
    pass


class UpdateCriterias(Struct):
    pass


class ArchiveCriterias(Struct):
    pass


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

    failed_candidates = []
    for candidate in items:
        candidate.setdefault("id", get_id())
        try:
            convert(candidate, Candidate)
        except ValidationError:
            failed_candidates.append({**candidate})
            continue
        CANDIDATES_DB.append({**candidate, "status": "created"})

    adapter.info("Finished create operation")
    return failed_candidates


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

    failed_candidates = []
    for candidate in items:
        index, candidate_to_update = next(
            (
                (index, _candidate)
                for index, _candidate in enumerate(CANDIDATES_DB)
                if _candidate["id"] == candidate["id"]
            ),
            (None, None),
        )
        if candidate_to_update is None or index is None:
            continue

        updated = {**candidate_to_update, **candidate, "status": "updated"}
        try:
            convert(updated, Candidate)
        except ValidationError:
            failed_candidates.append(candidate)
            continue
        CANDIDATES_DB[index] = updated

    adapter.info("Finished update operation")
    return failed_candidates


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

    for candidate in items:
        index, candidate_to_archive = next(
            (
                (index, _candidate)
                for index, _candidate in enumerate(CANDIDATES_DB)
                if _candidate["id"] == candidate["id"]
            ),
            (None, None),
        )
        if candidate_to_archive is None or index is None:
            continue

        CANDIDATES_DB[index]["status"] = "archived"

    adapter.info("Finished archive operation")
    return []


CandidatesAisle = Aisle(
    name=Entity.profile,
    write=WriteOperation(
        function=merge(create=create, update=update, archive=archive),
        criterias=Criterias(
            create=CreateCriterias, update=UpdateCriterias, archive=ArchiveCriterias
        ),
    ),
    schema=Candidate,
)
