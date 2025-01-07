import typing as t

from hrflow_connectors.v2.connectors.meteojob.warehouse import MeteojobWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def rename_profile_fields(meteojob_profile: t.Dict) -> t.Dict:
    return {
        "job-number": meteojob_profile.get("jobkey", "")[:10] or None,
        "first_name": meteojob_profile.get("firstName"),
        "last_name": meteojob_profile.get("lastName"),
        "phone": meteojob_profile.get("phone"),
        "email": meteojob_profile.get("email"),
        "coverText": meteojob_profile.get("coverText"),
        "profile-country": meteojob_profile.get("profilecountry"),
        "profile-regions": meteojob_profile.get("profileregions"),
        "profile-domains": meteojob_profile.get("profiledomains"),
        "job-lien_annonce_site_carriere": meteojob_profile.get(
            "joblien_annonce_site_carriere"
        ),
        "statistic-source": meteojob_profile.get("statisticsource"),
        "statistic-jbsource": meteojob_profile.get("statisticjbsource"),
    }


def add_tags(profile_tags: t.Dict) -> t.List[t.Dict]:
    return [dict(name=key, value=value) for key, value in profile_tags.items() if value]


def format_meteojob_profile(meteojob_profile: t.Dict) -> t.Dict:
    profile_tags = rename_profile_fields(meteojob_profile)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=meteojob_profile["cv"],
        content_type=meteojob_profile["content_type"],
    )
    return dict(
        reference=meteojob_profile["email"],
        resume=resume_dict,
        tags=tags,
        metadatas=[],
    )


def format_meteojob_profile_for_archive(meteojob_profile: t.Dict) -> t.Dict:
    return dict(reference=meteojob_profile["email"])


def event_parser(event: t.Dict) -> t.Dict:
    return dict(profile=event)


DESCRIPTION = "METEOJOB: EUROPEAN LEADER IN MATCHING, BIG DATA AND HR VIDEO INTERVIEWS"

Meteojob = Connector(
    name="Meteojob",
    type=ConnectorType.JobBoard,
    subtype="meteojob",
    description=DESCRIPTION,
    url="https://www.meteojob.com/",
    warehouse=MeteojobWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_meteojob_profile,
            event_parser=event_parser,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_meteojob_profile,
            event_parser=event_parser,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_meteojob_profile_for_archive,
            event_parser=event_parser,
        ),
    ),
)
