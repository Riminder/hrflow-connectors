import typing as t

from hrflow_connectors.v2.connectors.jobology.warehouse import JobologyWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def rename_profile_fields(jobology_profile: t.Dict) -> t.Dict:
    return {
        "job-number": jobology_profile.get("jobkey", "")[:10] or None,
        "first_name": jobology_profile.get("firstName"),
        "last_name": jobology_profile.get("lastName"),
        "phone": jobology_profile.get("phone"),
        "email": jobology_profile.get("email"),
        "coverText": jobology_profile.get("coverText"),
        "profile-country": jobology_profile.get("profilecountry"),
        "profile-regions": jobology_profile.get("profileregions"),
        "profile-domains": jobology_profile.get("profiledomains"),
        "job-lien_annonce_site_carriere": jobology_profile.get(
            "joblien_annonce_site_carriere"
        ),
        "statistic-source": jobology_profile.get("statisticsource"),
        "statistic-jbsource": jobology_profile.get("statisticjbsource"),
    }


def add_tags(profile_tags: t.Dict) -> t.List[t.Dict]:
    return [dict(name=key, value=value) for key, value in profile_tags.items() if value]


def format_jobology_profile(jobology_profile: t.Dict) -> t.Dict:
    profile_tags = rename_profile_fields(jobology_profile)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=jobology_profile["cv"],
        content_type=jobology_profile["content_type"],
    )
    return dict(
        reference=jobology_profile["email"],
        resume=resume_dict,
        tags=tags,
        metadatas=[],
    )


def format_jobology_profile_for_archive(jobology_profile: t.Dict) -> t.Dict:
    return dict(reference=jobology_profile["email"])


def event_parser(event: t.Dict) -> t.Dict:
    return dict(profile=event)


DESCRIPTION = (
    "Jobology helps ambitious managers grow their businesses by building the right"
    " teams through a new approach to recruitment."
)

Jobology = Connector(
    name="Jobology",
    type=ConnectorType.JobBoard,
    subtype="jobology",
    description=DESCRIPTION,
    url="https://www.jobology.fr/",
    warehouse=JobologyWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_jobology_profile,
            event_parser=event_parser,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_jobology_profile,
            event_parser=event_parser,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_jobology_profile_for_archive,
            event_parser=event_parser,
        ),
    ),
)
