import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowProfileWarehouse
from hrflow_connectors.connectors.waalaxy.warehouse import WaalaxyProfilesWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


def format_waalaxy_profile(waalaxy_profile: t.Dict) -> t.Dict:
    tags = [
        dict(name="prospectList", value=waalaxy_profile["prospectList"]),
        dict(name="messageSent", value=waalaxy_profile["messageSent"]),
        dict(name="messageReplied", value=waalaxy_profile["messageReplied"]),
        dict(name="emailSent", value=waalaxy_profile["emailSent"]),
        dict(name="emailReplied", value=waalaxy_profile["emailReplied"]),
    ]
    info = dict(
        first_name=waalaxy_profile["firstName"],
        last_name=waalaxy_profile["lastName"],
        full_name=" ".join([waalaxy_profile["firstName"], waalaxy_profile["lastName"]]),
        email=waalaxy_profile["email"],
        phone=waalaxy_profile["phoneNumbers"],
        location={"text": waalaxy_profile["location"], "lat": "", "lng": ""},
        urls=[
            dict(type="linkedin", url=waalaxy_profile["linkedinUrl"]),
            dict(type="company_linkedin", url=waalaxy_profile["company_linkedinUrl"]),
            dict(type="company_website", url=waalaxy_profile["company_website"]),
            dict(type="salesNavigator", url=waalaxy_profile["salesNavigatorUrl"]),
        ],
        summary=waalaxy_profile["occupation"],
    )

    return dict(
        reference=waalaxy_profile["_id"],
        tags=tags,
        info=info,
        text_language="en",
        experiences=[],
        educations=[],
    )


def event_parser(event: t.Dict) -> t.Dict:
    return dict(profile=event)


DESCRIPTION = "Waalaxy"
Waalaxy = Connector(
    name="Waalaxy",
    type=ConnectorType.Automation,
    description=DESCRIPTION,
    url="https://www.waalaxy.com/fr/",
    actions=[
        ConnectorAction(
            name=ActionName.catch_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Imports the visited profiles, in synchronization with the Waalaxy"
                " campaign (Visit + CRM Sync)"
            ),
            parameters=BaseActionParameters.with_defaults(
                "TriggerViewActionParameters",
                format=format_waalaxy_profile,
                event_parser=event_parser,
            ),
            origin=WaalaxyProfilesWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        )
    ],
)
