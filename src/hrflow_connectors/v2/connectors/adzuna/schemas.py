import datetime
import typing as t
from enum import Enum

import msgspec
from msgspec import Struct


class Category(Struct):
    tag: str
    label: str


class Location(msgspec.Struct):
    area: t.List[str]
    display_name: str


class ContractType(str, Enum):
    PERMANENT = "permanent"
    CONTRACT = "contract"


class Flag(str, Enum):
    YES = "1"
    NO = "0"


class ContractTime(str, Enum):
    FULL_TIME = "full_time"
    PART_TIME = "part_time"


class Company(msgspec.Struct):
    display_name: str
    canonical_name: t.Optional[str]
    count: t.Optional[int]


class AdzunaJob(Struct):
    id: str
    created: str
    title: str
    description: str
    full_description: t.Optional[str]
    redirect_url: str
    latitude: t.Optional[float]
    longitude: t.Optional[float]
    category: Category
    location: Location
    salary_min: int
    salary_max: int
    salary_is_predicted: Flag
    company: Company
    contract_type: t.Optional[ContractType]
    contract_time: t.Optional[ContractTime]

    def __post_init__(self):
        self._validate_created()

    def _validate_created(self):
        try:
            datetime.date.fromisoformat(self.created)
        except ValueError:
            raise ValueError(
                f"Invalid time format given '{self.created}', expected ISO 8601 format"
                " (YYYY-MM-DDTHH:MM:SSZ)"
            )
