import datetime
import re
import typing as t
from enum import Enum

from pydantic import BaseModel, validator

MAX_CLIENT_REF = 30
MAX_TITLE = 100
MIN_TITLE = 1
MAX_DESCRIPTION = 10000
FORMAT = "%Y-%m-%d"
NUM_TIME_TYPES = 2
NUMBER_BUSINESS_SECTORS = 17
NUM_CONTRACT_TYPES = 6
NUM_OCCUPATIONS = 20
MAX_SALARY = 999999999
MAX_LENGTH_PHONE = 20
MAX_LENGTH_EMAIL = 255
MAX_LENGTH_COMPANY_NAME = 50
MAX_LENGTH_COMPANY_URL = 255
MAX_DEGREE = 6
NUM_MAX_PICTURES = 5


class DurationTypes(Enum):
    DAY = "day"
    WEEK = "week"
    MONTH = "month"
    YEAR = "year"


class SalaryPer(Enum):
    HOUR = "hour"
    DAY = "day"
    WEEK = "week"
    MONTH = "month"
    YEAR = "year"


class Experience(Enum):
    EXP1: "1"
    EXP2: "3"
    EXP3: "5"


class ContractDuration(BaseModel):
    min: t.Optional[int]
    max: t.Optional[int]
    duration_type: t.Optional[str]

    @validator("duration_type")
    def is_duration_valid(cls, v):
        if v is None:
            v = DurationTypes.YEAR
        if v not in DurationTypes:
            raise ValueError("Invalid duration type")
        return True


class Location(BaseModel):
    street: t.Optional[str]
    zip_code: str
    city: str
    country: t.Optional[str]


class Salary(BaseModel):
    min: t.Optional[float]
    max: t.Optional[float]
    per: t.Optional[str]
    benefits: t.Optional[str]

    @validator("per")
    def is_valid_per(cls, v):
        if v not in SalaryPer:
            error_message = "Salary must be per " + " ".join(
                [e.value + " or" for e in SalaryPer]
            )
            raise ValueError(error_message[:-2])


class Job(BaseModel):
    client_reference: t.Optional[str]
    title: str
    description: str
    start_date: t.Optional[datetime.date]
    time_type: t.Optional[str]
    contract_type: str
    business_sector: t.Optional[str]
    occupation: t.Optional[str]
    contract_duration: t.Optional[ContractDuration]
    location: Location
    salary: t.Optional[Salary]

    @validator("client_reference")
    def maxChar(cls, v):
        if len(v) > MAX_CLIENT_REF:
            raise ValueError("Must not exceed " + str(MAX_CLIENT_REF) + " characters")
        return v

    @validator("title")
    def titleMaxChar(cls, v):
        if len(v) < MIN_TITLE or len(v) > MAX_TITLE:
            raise ValueError(
                "Title length must be between "
                + str(MIN_TITLE)
                + " and "
                + str(MAX_TITLE)
                + " characters"
            )
        return v

    @validator("description")
    def description_limit(cls, v):
        if len(v) > MAX_DESCRIPTION:
            raise ValueError(
                "Description must not exceed " + str(MAX_DESCRIPTION) + " characters"
            )
        return v

    @validator("start_date")
    def in_iso_format(cls, v):
        date_string = v.isoformat()
        try:
            datetime.datetime.strptime(date_string, format=FORMAT)
        except ValueError:
            raise ValueError("Incorrect date format")
        return v

    @validator("time_type")
    def is_acceptable_time_type(cls, v):
        if int(v) not in range(1, NUM_TIME_TYPES):
            raise ValueError("Invalid Time Type")
        return True

    @validator("contract_type")
    def is_valid_contract_type(cls, v):
        if int(v) not in range(1, NUM_CONTRACT_TYPES):
            raise ValueError("Invalid Contract Type")
        return True

    @validator("business_sector")
    def is_valid_business_sector(cls, v):
        if int(v) not in range(1, NUMBER_BUSINESS_SECTORS):
            raise ValueError("Invalid Business Sector")
        return True

    @validator("occupation")
    def is_valid_occupation(cls, v):
        if int(v) not in range(1, NUM_OCCUPATIONS):
            raise ValueError("Invalid Occupation")
        return True

    @validator("salary")
    def is_valid_salary(cls, v):
        if v.max > MAX_SALARY:
            raise ValueError("Invalid Salary")
        return True


class Application(BaseModel):
    mode: str
    contact: str

    @validator("mode")
    def is_mode_valid(cls, v):
        if v not in ["url", "phone", "email", "direct apply"]:
            raise ValueError("Invalid Mode")
        return True


class Contact(BaseModel):
    name: t.Optional[str]
    phone_number: t.Optional[str]
    email: t.Optional[str]

    @validator("phone_number")
    def is_phone_number_valid(cls, v):
        if len(v) > MAX_LENGTH_PHONE:
            raise ValueError("Invalid Phone Number")
        return True

    @validator("email")
    def is_email_valid(cls, v):
        if len(v) > MAX_LENGTH_EMAIL:
            raise ValueError("Invalid Email Address")
        return True


class Company(BaseModel):
    name: t.Optional[str]
    description: t.Optional[str]
    url: t.Optional[str]
    location: t.Optional[Location]
    contact: t.Optional[Contact]

    @validator("name")
    def is_name_valid(cls, v):
        if len(v) > MAX_LENGTH_COMPANY_NAME:
            raise ValueError(
                "Name must not exceed " + str(MAX_LENGTH_COMPANY_NAME) + "characters"
            )
        return True

    @validator("url")
    def is_url_valid(cls, v):
        if len(v) > MAX_LENGTH_COMPANY_URL:
            raise ValueError(
                "URL must not exceed " + str(MAX_LENGTH_COMPANY_URL) + " characters"
            )
        return True


class Applicant(BaseModel):
    profile: t.Optional[str]
    skills: t.Optional[str]
    degree: t.Optional[str]
    experience: t.Optional[str]

    @validator("degree")
    def is_degree_valid(cls, v):
        if int(v) not in range(1, MAX_DEGREE):
            raise ValueError("Invalid degree value")
        return True

    @validator("experience")
    def is_experience_valid(cls, v):
        if v not in Experience:
            raise ValueError("Invalid experience value")
        return True


class Ad(BaseModel):
    morpheus_client_id: int
    partner_unique_reference: str
    job: Job
    application: Application
    company: t.Optional[Company]
    applicant: t.Optional[Applicant]
    pictures: t.Optional[str]

    @validator("partner_unique_reference")
    def must_not_contain_dot(cls, v):
        if "." in v:
            raise ValueError("Must not contain a dot")

    @validator("pictures")
    def are_pictures_valid(cls, v):
        if len(v) > NUM_MAX_PICTURES:
            raise ValueError(
                "Must have less than " + str(NUM_MAX_PICTURES) + " elements"
            )
        for i in range(len(v)):
            r_image = re.compile(r".*\.(jpeg|png)$")
            if not r_image.match(v[i]):
                raise ValueError("Image must be in JPEG or PNG format")
        return True
