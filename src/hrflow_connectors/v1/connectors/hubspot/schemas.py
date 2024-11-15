import typing as t

from pydantic import BaseModel, Field


class Properties(BaseModel):
    email: str
    firstname: str
    lastname: str
    date_of_birth: t.Optional[str]
    phone: t.Optional[str]
    address: t.Optional[str]
    zip: t.Optional[str]
    city: t.Optional[str]
    state: t.Optional[str]
    country: t.Optional[str]
    jobtitle: t.Optional[str]
    company: t.Optional[str]
    annualrevenue: t.Optional[str]
    website: t.Optional[str]


class ContactObject(BaseModel):
    properties: Properties = Field(
        ...,
        description=(
            "Contact details are stored in contact properties. In addition to default"
            " properties, you can store custom data by creating custom contact"
            " properties. These can be managed through the CRM object properties"
            " endpoints."
        ),
    )
