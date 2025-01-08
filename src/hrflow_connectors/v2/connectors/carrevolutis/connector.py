import typing as t

from hrflow_connectors.v2.connectors.carrevolutis.warehouse import CarrevolutisWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def rename_profile_fields(carrevolutis_profile: t.Dict) -> t.Dict:
    return {
        "job-number": carrevolutis_profile.get("jobkey", "")[:10] or None,
        "first_name": carrevolutis_profile.get("firstName"),
        "last_name": carrevolutis_profile.get("lastName"),
        "phone": carrevolutis_profile.get("phone"),
        "email": carrevolutis_profile.get("email"),
        "coverText": carrevolutis_profile.get("coverText"),
        "profile-country": carrevolutis_profile.get("profilecountry"),
        "profile-regions": carrevolutis_profile.get("profileregions"),
        "profile-domains": carrevolutis_profile.get("profiledomains"),
        "job-lien_annonce_site_carriere": carrevolutis_profile.get(
            "joblien_annonce_site_carriere"
        ),
        "statistic-source": carrevolutis_profile.get("statisticsource"),
        "statistic-jbsource": carrevolutis_profile.get("statisticjbsource"),
    }


def add_tags(profile_tags: t.Dict) -> t.List[t.Dict]:
    return [dict(name=key, value=value) for key, value in profile_tags.items() if value]


def format_carrevolutis_profile(carrevolutis_profile: t.Dict) -> t.Dict:
    profile_tags = rename_profile_fields(carrevolutis_profile)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=carrevolutis_profile["cv"],
        content_type=carrevolutis_profile["content_type"],
    )
    return dict(
        reference=carrevolutis_profile["email"],
        resume=resume_dict,
        tags=tags,
        metadatas=[],
    )


def format_carrevolutis_profile_for_archive(carrevolutis_profile: t.Dict) -> t.Dict:
    return dict(reference=carrevolutis_profile["email"])


def event_parser(event: t.Dict) -> t.Dict:
    return dict(profile=event)


DESCRIPTION = (
    "Carrevolutis' core business is recruitment in France, along with IT solutions and"
    " financial advice."
)
Carrevolutis = Connector(
    name="Carrevolutis",
    type=ConnectorType.JobBoard,
    subtype="carrevolutis",
    description=DESCRIPTION,
    url="https://www.carrevolutis.com/",
    warehouse=CarrevolutisWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_carrevolutis_profile,
            event_parser=event_parser,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_carrevolutis_profile,
            event_parser=event_parser,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_carrevolutis_profile_for_archive,
            event_parser=event_parser,
        ),
    ),
)
