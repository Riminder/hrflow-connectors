import typing as t

from msgspec import Struct


class Document(Struct):
    filename: str
    type: str
    content: str


class Transaction(Struct):
    client_id: str
    source_id: str


class Context(Struct):
    id: str
    shortlist_id: t.Optional[str]
    aplitrak_email_address: t.Optional[str]


class BroadbeanCandidate(Struct):
    name: str
    first_name: t.Optional[str]
    last_name: t.Optional[str]
    email: str
    contact_telephone: t.Optional[str]
    mobile_telephone: t.Optional[str]
    location_city: t.Optional[str]
    location_country: t.Optional[str]
    location_postcode: t.Optional[str]
    location_latitude: t.Optional[str]
    location_longitude: t.Optional[str]
    current_job_title: t.Optional[str]
    current_job_employer: t.Optional[str]
    current_job_startdate: t.Optional[str]
    documents: t.Optional[Document]
