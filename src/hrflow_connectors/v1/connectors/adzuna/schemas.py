import datetime
import typing as t
from enum import Enum

from pydantic import BaseModel, validator


class Category(BaseModel):
    __CLASS__: str
    tag: str
    label: str


class Location(BaseModel):
    __CLASS__: str
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


class Company(BaseModel):
    __CLASS__: str
    display_name: str
    canonical_name: t.Optional[str]
    count: t.Optional[int]


class AdzunaJob(BaseModel):
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
    contract_time: ContractTime

    @validator("created")
    def validate_date(value: t.Any) -> str:
        try:
            _ = datetime.date.fromisoformat(value)
            return value
        except ValueError:
            raise ValueError(
                "Invalid time format given {}, expected format ISO 8601 format"
                " ISO-8601 (YYYY-MM-DDTHH:MM:SSZ)".format(value)
            )
