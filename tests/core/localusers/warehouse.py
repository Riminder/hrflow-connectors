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
]


class PullUsersParameters(BaseModel):
    gender: t.Optional[Gender] = None


def read(adapter: LoggerAdapter, parameters: PullUsersParameters) -> t.Iterable[t.Dict]:
    if parameters.gender is None:
        adapter.info("Returning all users")
        return USERS_DB[:]
    adapter.info("Returning {} users".format(parameters.gender))
    return [item for item in USERS_DB if item["gender"] is parameters.gender]


UsersWarehouse = Warehouse(
    name="Test Users",
    data_schema=User,
    read=WarehouseReadAction(
        parameters=PullUsersParameters,
        function=read,
        endpoints=[GET_USERS],
    ),
)

BadUsersWarehouse = Warehouse(
    name="Bad Test Users",
    data_schema=User,
    read=WarehouseReadAction(
        parameters=PullUsersParameters,
        function=lambda *args, **kwargs: 10 / 0,
        endpoints=[GET_USERS],
    ),
)
