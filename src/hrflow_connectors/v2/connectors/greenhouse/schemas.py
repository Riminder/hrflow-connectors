from typing import Any, List, Optional

from msgspec import Meta, Struct
from typing_extensions import Annotated


# Job Model
class Location(Struct):
    name: str


class Department(Struct):
    id: int
    name: str
    parent_id: Any
    child_ids: Optional[List[int]]
    external_id: int


class Office(Struct):
    id: int
    name: str
    location: str
    parent_id: Optional[int]
    child_ids: Optional[List[int]]


class Salary_range(Struct):
    min_value: int
    max_value: int
    unit: str


class Custom_fields(Struct):
    employment_type: str
    maximum_budget: str
    salary_range: Salary_range


class Employement_type(Struct):
    name: str
    type: str
    value: str


class Budget(Struct):
    name: str
    type: str
    value: str


class Keyed_custom_fields(Struct):
    employement_type = Employement_type
    budget: Budget
    salary_range: Salary_range


class Team_model(Struct):
    id: int
    first_name: str
    last_name: str
    name: str
    employee_id: str
    responsible: Optional[bool]


class Hiring_team(Struct):
    hiring_managers: List[Team_model]
    recruiters: List[Team_model]
    coordinators: List[Team_model]
    sourcers: List[Team_model]


class Close_reason(Struct):
    id: int
    name: str


class Opening(Struct):
    id: int
    opening_id: str
    status: str
    opened_at: str
    closed_at: str
    application_id: int
    close_reason: Close_reason


class GreenhouseJobModel(Struct):
    id: int
    requisition_id: str
    status: str
    confidential: bool
    departments: List[Department]
    offices: List[Office]
    hiring_team: Hiring_team
    custom_fields: Custom_fields
    keyed_custom_fields: Keyed_custom_fields
    openings: List[Opening]
    is_template: bool
    copied_from_id: int
    created_at: str
    updated_at: str


# Profile Model
class PhoneNumber(Struct):
    value: str
    type: str


class Address(Struct):
    value: str
    type: str


class EmailAddress(Struct):
    value: str
    type: str


class WebsiteAddress(Struct):
    value: str
    type: str


class SocialMediaAddress(Struct):
    value: str


class Education(Struct):
    school_id: int
    discipline_id: int
    degree_id: int
    start_date: str
    end_date: str


class Employment(Struct):
    company_name: str
    title: str
    start_date: str
    end_date: str


class Recruiter(Struct):
    id: Optional[int]
    email: Optional[str]


class coordinator(Struct):
    id: int
    email: str


class GreenhouseProfileModel(Struct):
    id: Annotated[int, Meta(description="The candidate's unique identifier")]
    first_name: Annotated[str, Meta(description="The candidate's first name")]
    last_name: Annotated[str, Meta(description="The candidate's last name")]
    company: Annotated[
        str, Meta(description="The company at which the candidate currently works")
    ]
    title: Annotated[str, Meta(description="The candidate's current title")]
    is_private: Annotated[
        bool, Meta(description="Whether the candidate is private or not")
    ]
    application_ids: Annotated[
        List[int],
        Meta(
            description=(
                "Array of application IDs associated with this candidate. Can contain"
                " none, one, or several application IDs"
            )
        ),
    ]
    phone_numbers: Annotated[
        List[PhoneNumber], Meta(description="The candidate's phone numbers")
    ]
    addresses: Annotated[List[Address], Meta(description="The candidate's addresses")]
    email_addresses: Annotated[
        List[EmailAddress], Meta(description="The candidate's email addresses")
    ]
    website_addresses: Annotated[
        List[WebsiteAddress], Meta(description="The candidate's website addresses")
    ]
    social_media_addresses: Annotated[
        List[SocialMediaAddress],
        Meta(description="The candidate's social media addresses"),
    ]
    recruiter: Annotated[
        Recruiter,
        Meta(description="The recruiter user who is responsible for this candidate"),
    ]
    coordinator: Annotated[
        coordinator,
        Meta(description="The coordinator user who is responsible for this candidate"),
    ]
    attachments: Annotated[List[Any], Meta(description="The candidate's attachments")]
    custom_fields: Annotated[
        Custom_fields,
        Meta(
            description=(
                "Contains a hash of the custom fields configured for this resource"
            )
        ),
    ]
    keyed_custom_fields: Annotated[
        Keyed_custom_fields,
        Meta(
            description=(
                "Contains the same information as custom_fields but formatted in a"
                " different way that includes more information"
            )
        ),
    ]
    linked_user_ids: Annotated[
        List[int],
        Meta(
            description=(
                "If the candidate is an internal applicant, this returns the Greenhouse"
                " user ID of the candidate"
            )
        ),
    ]
