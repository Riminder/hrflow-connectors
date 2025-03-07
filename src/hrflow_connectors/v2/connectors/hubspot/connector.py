import typing as t

from hrflow_connectors.v2.connectors.hubspot.warehouse import HubspotWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_profile_info = hrflow_profile["info"]
    hrflow_location = hrflow_profile_info["location"]
    fields = hrflow_location["fields"] or {}
    properties = dict(
        email=hrflow_profile_info["email"],
        firstname=hrflow_profile_info["first_name"],
        lastname=hrflow_profile_info["last_name"],
        date_of_birth=hrflow_profile_info["date_birth"],
        phone=hrflow_profile_info["phone"],
        address=hrflow_location["text"],
        zip=fields.get("postcode") or "Undefined",
        city=fields.get("city") or "Undefined",
        state=fields.get("state") or "Undefined",
        country=fields.get("country") or "Undefined",
        company="",
    )
    hubspot_contact = dict(properties=properties)
    return hubspot_contact


def format_hrflow_profile_for_update(hrflow_profile: t.Dict) -> t.Dict:
    hubspot_contact = format_hrflow_profile(hrflow_profile)
    hubspot_contact["contactId"] = hrflow_profile["reference"]
    return hubspot_contact


def format_hrflow_profile_for_archive(hrflow_profile: t.Dict) -> t.Dict:
    hubspot_contact = dict(contactId=hrflow_profile["reference"])
    return hubspot_contact


def format_hubspot_contact(hubspot_contact: t.Dict) -> t.Dict:
    properties = hubspot_contact["properties"]
    profile = dict(
        reference=hubspot_contact["id"],
        info=dict(
            email=properties["email"],
            first_name=properties["firstname"],
            last_name=properties["lastname"],
            full_name=f"{properties['firstname']} {properties['lastname']}",
            date_birth=properties["date_of_birth"],
            phone=properties["phone"],
            location=dict(
                text=properties["address"],
                fields=dict(
                    postcode=properties["zip"],
                    city=properties["city"],
                    state=properties["state"],
                    country=properties["country"],
                ),
            ),
        ),
        created_at=hubspot_contact["createdAt"],
        updated_at=hubspot_contact["updatedAt"],
        experiences=[],
        educations=[],
    )
    return profile


def format_hubspot_contact_for_archive(hubspot_contact: t.Dict) -> t.Dict:
    return dict(reference=hubspot_contact["id"])


Hubspot = Connector(
    name="Hubspot",
    type=ConnectorType.CRM,
    subtype="hubspot",
    description=(
        "HubSpot is a CRM platform with all the software, integrations, and resources"
        " you need to connect marketing, sales, content management, and customer"
        " service."
    ),
    url="https://www.hubspot.com/",
    warehouse=HubspotWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_hubspot_contact,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_hubspot_contact,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_for_update,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_hubspot_contact_for_archive,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_for_archive,
        ),
    ),
)
