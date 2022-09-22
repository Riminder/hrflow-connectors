import enum
import typing as t
from logging import LoggerAdapter

from pydantic import BaseModel

from hrflow_connectors.core import (
    ActionEndpoints,
    DataType,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)

GET_USERS = ActionEndpoints(
    name="Get all user",
    description="Get all users USERS_DB",
    url="https://local_user.readthedocs.com/users",
)

FAIL_AT = 7


class Gender(enum.Enum):
    male = "male"
    female = "female"


class User(BaseModel):
    name: str
    email: str
    gender: Gender


USERS_DB = [
    dict(id=1, name="Arthur", email="arthur@mailbox.com", gender=Gender.male),
    dict(id=2, name="Patrick", email="patrick@mailbox.com", gender=Gender.male),
    dict(id=3, name="Emilie", email="emilie@mailbox.com", gender=Gender.female),
    dict(id=4, name="Nicole", email="nicole@mailbox.com", gender=Gender.female),
    dict(id=5, name="Jean", email="jean@mailbox.com", gender=Gender.male),
    dict(id=6, name="Durant", email="durant@mailbox.com", gender=Gender.male),
    dict(id=7, name="Sarah", email="sarah@mailbox.com", gender=Gender.female),
    dict(id=8, name="Beatrice", email="beatrice@mailbox.com", gender=Gender.female),
    dict(id=9, name="Marc", email="marc@mailbox.com", gender=Gender.male),
]


def add_user():
    last_id = USERS_DB[-1]["id"]
    new_id = last_id + 1
    new_user = dict(
        id=new_id,
        name="AddedUser{}".format(new_id),
        email="added.user.{}@mailbox.com".format(new_id),
        gender=Gender.male,
    )
    USERS_DB.append(new_user)
    return new_user


class ReadUsersParameters(BaseModel):
    gender: t.Optional[Gender] = None


def read(
    adapter: LoggerAdapter,
    parameters: ReadUsersParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    if parameters.gender is None:
        adapter.info("Returning all users")
        if read_mode is ReadMode.incremental and read_from is not None:
            return [item for item in USERS_DB if item["id"] > int(read_from)]
        return USERS_DB[:]

    adapter.info("Returning {} users".format(parameters.gender))
    if read_mode is ReadMode.incremental and read_from is not None:
        return [
            item
            for item in USERS_DB
            if item["gender"] is parameters.gender and item["id"] > read_from
        ]
    return [item for item in USERS_DB if item["gender"] is parameters.gender]


def read_with_failures(
    adapter: LoggerAdapter,
    parameters: ReadUsersParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    if parameters.gender is None:
        adapter.info("Returning all users")
        users_to_return = USERS_DB[:]
    else:
        adapter.info("Returning {} users".format(parameters.gender))
        users_to_return = [
            item for item in USERS_DB if item["gender"] is parameters.gender
        ]
    for i, user in enumerate(users_to_return):
        if i == FAIL_AT:
            (10 / 0)
        yield user


UsersWarehouse = Warehouse(
    name="Test Users",
    data_schema=User,
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadUsersParameters,
        function=read,
        endpoints=[GET_USERS],
    ),
)


UsersIncrementalWarehouse = Warehouse(
    name="Test Users",
    data_schema=User,
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadUsersParameters,
        function=read,
        endpoints=[GET_USERS],
        supports_incremental=True,
        item_to_read_from=lambda item: str(item["id"]),
    ),
)


FailingUsersWarehouse = Warehouse(
    name="Test Users",
    data_schema=User,
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadUsersParameters,
        function=read_with_failures,
        endpoints=[GET_USERS],
    ),
)

BadUsersWarehouse = Warehouse(
    name="Bad Test Users",
    data_schema=User,
    data_type=DataType.other,
    read=WarehouseReadAction(
        parameters=ReadUsersParameters,
        function=lambda *args, **kwargs: 10 / 0,
        endpoints=[GET_USERS],
    ),
)
