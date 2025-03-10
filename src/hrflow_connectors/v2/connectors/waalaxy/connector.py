import typing as t

from hrflow_connectors.v2.connectors.waalaxy.warehouse import WaalaxyWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


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


def format_archive(waalaxy_profile: t.Dict) -> t.Dict:
    return dict(reference=waalaxy_profile["_id"])


def event_parser(event: t.Dict) -> t.Dict:
    return dict(profile=event)


Waalaxy = Connector(
    name="Waalaxy",
    type=ConnectorType.Automation,
    subtype="waalaxy",
    description="The perfect Tool for Lead Generation on LinkedIn",
    url="https://www.waalaxy.com/fr/",
    warehouse=WaalaxyWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_waalaxy_profile,
            event_parser=event_parser,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_waalaxy_profile,
            event_parser=event_parser,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_archive,
            event_parser=event_parser,
        ),
    ),
)
