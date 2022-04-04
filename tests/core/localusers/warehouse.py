import enum
import typing as t
from logging import LoggerAdapter

from pydantic import BaseModel

from hrflow_connectors.core import ActionEndpoints, Warehouse, WarehouseReadAction

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
    dict(name="Arthur", email="arthur@mailbox.com", gender=Gender.male),
    dict(name="Patrick", email="patrick@mailbox.com", gender=Gender.male),
    dict(name="Emilie", email="emilie@mailbox.com", gender=Gender.female),
    dict(name="Nicole", email="nicole@mailbox.com", gender=Gender.female),
    dict(name="Jean", email="jean@mailbox.com", gender=Gender.male),
    dict(name="Durant", email="durant@mailbox.com", gender=Gender.male),
    dict(name="Sarah", email="sarah@mailbox.com", gender=Gender.female),
    dict(name="Beatrice", email="beatrice@mailbox.com", gender=Gender.female),
    dict(name="Marc", email="marc@mailbox.com", gender=Gender.male),
]


class ReadUsersParameters(BaseModel):
    gender: t.Optional[Gender] = None


def read(adapter: LoggerAdapter, parameters: ReadUsersParameters) -> t.Iterable[t.Dict]:
    if parameters.gender is None:
        adapter.info("Returning all users")
        return USERS_DB[:]
    adapter.info("Returning {} users".format(parameters.gender))
    return [item for item in USERS_DB if item["gender"] is parameters.gender]


def read_with_failures(
    adapter: LoggerAdapter, parameters: ReadUsersParameters
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
    read=WarehouseReadAction(
        parameters=ReadUsersParameters,
        function=read,
        endpoints=[GET_USERS],
    ),
)


FailingUsersWarehouse = Warehouse(
    name="Test Users",
    data_schema=User,
    read=WarehouseReadAction(
        parameters=ReadUsersParameters,
        function=read_with_failures,
        endpoints=[GET_USERS],
    ),
)

BadUsersWarehouse = Warehouse(
    name="Bad Test Users",
    data_schema=User,
    read=WarehouseReadAction(
        parameters=ReadUsersParameters,
        function=lambda *args, **kwargs: 10 / 0,
        endpoints=[GET_USERS],
    ),
)
