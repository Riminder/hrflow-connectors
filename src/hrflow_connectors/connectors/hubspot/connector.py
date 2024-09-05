import typing as t

from hrflow_connectors.connectors.hrflow.warehouse.profile import HrFlowProfileWarehouse
from hrflow_connectors.connectors.hubspot.warehouse import HubspotContactWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


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
    profile = dict(properties=properties)
    return profile


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
        experiences=[],
        educations=[],
    )
    return profile


Hubspot = Connector(
    name="Hubspot",
    type=ConnectorType.CRM,
    subtype="hubspot",
    description="",
    url="https://www.hubspot.com/",
    actions=[
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from Hrflow.ai Source as a contact on Hubspot via"
                " the API"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_hrflow_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=HubspotContactWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves contacts from Hubspot via API and send them"
                " to a ***Hrflow.ai Source***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfilessActionParameters", format=format_hubspot_contact
            ),
            origin=HubspotContactWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
