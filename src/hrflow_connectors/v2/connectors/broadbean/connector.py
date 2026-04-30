import base64
import typing as t

import requests

from hrflow_connectors.v2.connectors.broadbean.warehouse import BroadbeanWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_profile_city(hrflow_location: t.Dict) -> str:
    fields = hrflow_location["fields"] or {}
    return fields.get("city") or "Undefined"


def get_profile_country(hrflow_location: t.Dict) -> str:
    fields = hrflow_location["fields"] or {}
    return fields.get("country") or "Undefined"


def get_profile_postcode(hrflow_location: t.Dict) -> str:
    fields = hrflow_location["fields"] or {}
    return fields.get("postcode") or "Undefined"


def format_profile_push(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_profile_info = hrflow_profile["info"]
    profile = dict(
        name=hrflow_profile_info["full_name"],
        email=hrflow_profile_info["email"],
        contact_telephone=hrflow_profile_info["phone"] or "Udefined",
        mobile_telephone=hrflow_profile_info["phone"] or "Undefiend",
        location_city=get_profile_city(hrflow_profile_info["location"]),
        location_country=get_profile_country(hrflow_profile_info["location"]),
        location_postcode=get_profile_postcode(hrflow_profile_info["location"]),
        location_latitude=hrflow_profile_info["location"]["lat"] or 0,
        location_longitude=hrflow_profile_info["location"]["lng"] or 0,
        current_job_title=None,
        current_job_employer=None,
        current_job_startdate=None,
        documents=[
            dict(
                filename=attachment["filename"],
                type=attachment["type"],
                content=base64.b64encode(
                    requests.get(attachment["public_url"]).content
                ).decode("utf-8"),
            )
            for attachment in hrflow_profile["attachments"]
        ],
    )
    return profile


DESCRIPTION = (
    "Broadbean is a software platform for recruitment and talent acquisition. "
    "It helps companies and recruiters find, attract, and hire the best candidates "
    "for open positions. It offers features such as job posting, resume search, "
    "candidate tracking, and reporting and analytics. Broadbean is used by "
    "companies across a wide range of industries and is a popular choice for "
    "recruiters and talent acquisition professionals who want to streamline their "
    "recruitment process."
)

Broadbean = Connector(
    name="Broadbean",
    type=ConnectorType.ATS,
    subtype="broadbean",
    description=DESCRIPTION,
    url="https://www.broadbean.com",
    warehouse=BroadbeanWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_profile_push,
        ),
    ),
)
