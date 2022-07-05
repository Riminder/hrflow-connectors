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
    """Enumeration of contract duration types"""

    DAY = "day"
    WEEK = "week"
    MONTH = "month"
    YEAR = "year"


class SalaryPer(Enum):
    """Enumeration of different types of salary (per hour, per day ...)"""

    HOUR = "hour"
    DAY = "day"
    WEEK = "week"
    MONTH = "month"
    YEAR = "year"


class Experience(Enum):
    """Enumeration of different types of experience"""

    EXP1 = "1"
    EXP2 = "3"
    EXP3 = "5"


class ContactMode(Enum):
    """Enumeration of modes of contacting the recruiter"""

    URL = "url"
    PHONE = "phone"
    EMAIL = "email"
    DIRECT_APPLY = "direct apply"


class ContractDuration(BaseModel):
    """Schema model of ContractDuration Object"""

    min: t.Optional[int]
    max: t.Optional[int]
    duration_type: t.Optional[str]

    @validator("duration_type")  # duration_type should be one of DurationTypes
    def is_duration_valid(cls, v):
        """Checks if the contract duration type is valid

        Args:
            v (_type_): duration type as string

        Raises:
            ValueError: "Invalid duration type"_

        Returns:
            _type_: bool
        """
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

    @validator("per")  # salary should be per a value in SalaryPer
    def is_valid_per(cls, v):
        """Checks if the per field is valid ie per is in SalaryPer enumeration

        Args:
            v (_type_): per field as string
        """
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

    @validator("client_reference")  # limit on len(client_reference)
    def max_char(cls, v):
        """Checks if client_reference exceeds the maximum length."""
        if len(v) > MAX_CLIENT_REF:
            raise ValueError("Must not exceed " + str(MAX_CLIENT_REF) + " characters")
        return v

    @validator("title")  # limit on len(title)
    def title_limit_char(cls, v):
        """Checks if title length is between the maximum and minimum lengths."""
        if len(v) < MIN_TITLE or len(v) > MAX_TITLE:
            raise ValueError(
                "Title length must be between "
                + str(MIN_TITLE)
                + " and "
                + str(MAX_TITLE)
                + " characters"
            )
        return v

    @validator("description")  # limit on len(description)
    def description_limit(cls, v):
        """Checks if description length exceeds the maximum length."""
        if len(v) > MAX_DESCRIPTION:
            raise ValueError(
                "Description must not exceed " + str(MAX_DESCRIPTION) + " characters"
            )
        return v

    @validator("start_date")  # start_date should be in the format FORMAT
    def in_right_format(cls, v):
        """Checks if start_date is in the format FORMAT"""
        date_string = v.isoformat()
        try:
            datetime.datetime.strptime(date_string, format=FORMAT)
        except ValueError:
            raise ValueError("Incorrect date format")
        return v

    @validator("time_type")
    def is_valid_time_type(cls, v):
        """Checks if time_type value is in the range of allowed values"""
        if int(v) not in range(1, NUM_TIME_TYPES):
            raise ValueError("Invalid Time Type")
        return True

    @validator("contract_type")
    def is_valid_contract_type(cls, v):
        """Checks if contract type is in the range of allowed values"""
        if int(v) not in range(1, NUM_CONTRACT_TYPES):
            raise ValueError("Invalid Contract Type")
        return True

    @validator("business_sector")
    def is_valid_business_sector(cls, v):
        """Checks if business sector is in the range of allowed values"""
        if int(v) not in range(1, NUMBER_BUSINESS_SECTORS):
            raise ValueError("Invalid Business Sector")
        return True

    @validator("occupation")
    def is_valid_occupation(cls, v):
        """Checks if occupation is valid"""
        if int(v) not in range(1, NUM_OCCUPATIONS):
            raise ValueError("Invalid Occupation")
        return True

    @validator("salary")
    def is_valid_salary(cls, v):
        """Checks if salary exceeds the maximum value specified"""
        if v.max > MAX_SALARY:
            raise ValueError("Invalid Salary")
        return True


class Application(BaseModel):
    mode: str
    contact: str

    @validator("mode")
    def is_mode_valid(cls, v):
        """Checks if contact mode is valid : if it is in the ContactMode enumeration"""
        if v not in ContactMode:
            raise ValueError("Invalid Mode")
        return True


class Contact(BaseModel):
    name: t.Optional[str]
    phone_number: t.Optional[str]
    email: t.Optional[str]

    @validator("phone_number")  # limit on len(phone_number)
    def is_phone_number_valid(cls, v):
        """Checks if phone number is valid"""
        if len(v) > MAX_LENGTH_PHONE:
            raise ValueError("Invalid Phone Number")
        return True

    @validator("email")  # limit on len(email)
    def is_email_valid(cls, v):
        """Checks if the email address exceeds the maximum length"""
        if len(v) > MAX_LENGTH_EMAIL:
            raise ValueError("Invalid Email Address")
        return True


class Company(BaseModel):
    name: t.Optional[str]
    description: t.Optional[str]
    url: t.Optional[str]
    location: t.Optional[Location]
    contact: t.Optional[Contact]

    @validator("name")  # limit on len(name)
    def is_name_valid(cls, v):
        """Checks if name exceeds the maximum length allowed"""
        if len(v) > MAX_LENGTH_COMPANY_NAME:
            raise ValueError(
                "Name must not exceed " + str(MAX_LENGTH_COMPANY_NAME) + "characters"
            )
        return True

    @validator("url")  # limit on len(url)
    def is_url_valid(cls, v):
        """Checks if degree is valid"""
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
        """Checks if degree is valid"""
        if int(v) not in range(1, MAX_DEGREE):
            raise ValueError("Invalid degree value")
        return True

    @validator("experience")
    def is_experience_valid(cls, v):
        """Checks if experience if valid : is in Experience enumeration"""
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
    pictures: t.Optional[t.List[str]]

    @validator("partner_unique_reference")
    def must_not_contain_dot(cls, v):
        """Checks if partner_unique_reference contains a dot"""
        if "." in v:
            raise ValueError("Must not contain a dot")
        return True

    @validator("pictures")
    def are_pictures_valid(cls, v):
        """Checks if pictures are valid:
        - that they do not exceed the maximum number allowed
        - that they correspond to JPEG or PNG format

        Args:
            v (_type_): list of picture urls

        Raises:
            ValueError: _description_
            ValueError: _description_

        Returns:
            _type_: _description_
        """
        if len(v) > NUM_MAX_PICTURES:
            raise ValueError(
                "Must have less than " + str(NUM_MAX_PICTURES) + " elements"
            )
        for i in range(len(v)):
            r_image = re.compile(r".*\.(jpeg|png)$")
            if not r_image.match(v[i]):
                raise ValueError("Image must be in JPEG or PNG format")
        return True
