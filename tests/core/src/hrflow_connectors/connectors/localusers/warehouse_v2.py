import enum
import typing as t
from logging import LoggerAdapter

from pydantic import BaseModel, Field

from hrflow_connectors.core.warehouse_v2 import (
    ActionEndpoints,
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseType,
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


class AuthParameters(ParametersModel):
    api_secret: str = Field(None, repr=False, field_type=FieldType.Auth)
    api_user: str = Field(None, field_type=FieldType.Auth)


class ReadUsersParameters(ParametersModel):
    gender: t.Optional[Gender] = Field(None, field_type=FieldType.Other)


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: ReadUsersParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    if action_parameters.gender is None:
        adapter.info("Returning all users")
        if read_mode is ReadMode.incremental and read_from is not None:
            return [item for item in USERS_DB if item["id"] > int(read_from)]
        return USERS_DB[:]

    adapter.info("Returning {} users".format(action_parameters.gender))
    if read_mode is ReadMode.incremental and read_from is not None:
        return [
            item
            for item in USERS_DB
            if item["gender"] is action_parameters.gender and item["id"] > read_from
        ]
    return [item for item in USERS_DB if item["gender"] is action_parameters.gender]


def read_with_failures(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: ReadUsersParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    if action_parameters.gender is None:
        adapter.info("Returning all users")
        users_to_return = USERS_DB[:]
    else:
        adapter.info("Returning {} users".format(action_parameters.gender))
        users_to_return = [
            item for item in USERS_DB if item["gender"] is action_parameters.gender
        ]
    for i, user in enumerate(users_to_return):
        if i == FAIL_AT:
            (10 / 0)
        yield user


UsersWarehouse = Warehouse(
    name="Test Users",
    type=WarehouseType.outbound,
    data_schema=User,
    data_type=DataType.other,
    create=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=ReadUsersParameters,
        function=read,
        endpoints=[GET_USERS],
        supports_incremental=False,
    ),
)


UsersIncrementalWarehouse = Warehouse(
    name="Test Users",
    type=WarehouseType.outbound,
    data_schema=User,
    data_type=DataType.other,
    create=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=ReadUsersParameters,
        function=read,
        endpoints=[GET_USERS],
        supports_incremental=True,
        item_to_read_from=lambda item: str(item["id"]),
    ),
)


FailingUsersWarehouse = Warehouse(
    name="Test Users",
    type=WarehouseType.outbound,
    data_schema=User,
    data_type=DataType.other,
    update=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=ReadUsersParameters,
        function=read_with_failures,
        endpoints=[GET_USERS],
        supports_incremental=False,
    ),
)

BadUsersWarehouse = Warehouse(
    name="Bad Test Users",
    type=WarehouseType.outbound,
    data_schema=User,
    data_type=DataType.other,
    create=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=ReadUsersParameters,
        function=lambda *args, **kwargs: 10 / 0,
        endpoints=[GET_USERS],
        supports_incremental=False,
    ),
)
