import typing as t

from hrflow_connectors.v2.connectors.jobijoba.warehouse import JobijobaWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def rename_profile_fields(jobijoba_profile_event: t.Dict) -> t.Dict:
    job = jobijoba_profile_event.get("job", {})
    profile = jobijoba_profile_event.get("applicant", {})
    return {
        "job-number": job.get("jobId", "")[:10] or None,
        "first_name": profile.get("firstName"),
        "last_name": profile.get("lastName"),
        "phone": profile.get("phoneNumber"),
        "email": profile.get("email"),
        "job-lien_annonce_site_carriere": jobijoba_profile_event.get("jobAtsUrl"),
        "statistic-source": jobijoba_profile_event.get("source"),
        "statistic-jbsource": jobijoba_profile_event.get("source"),
    }


def add_tags(profile_tags: t.Dict) -> t.List[t.Dict]:
    return [dict(name=key, value=value) for key, value in profile_tags.items() if value]


def format_jobijoba_profile(jobijoba_profile_event: t.Dict) -> t.Dict:
    profile_tags = rename_profile_fields(jobijoba_profile_event)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=jobijoba_profile_event["cv"],
        content_type=jobijoba_profile_event["content_type"],
    )
    return dict(
        reference=jobijoba_profile_event["applicationId"],
        created_at=None,
        resume=resume_dict,
        tags=tags,
        metadatas=[],
    )


def format_jobijoba_profile_for_archive(jobijoba_profile: t.Dict) -> t.Dict:
    return dict(
        reference=jobijoba_profile["applicationId"],
    )


def event_parser(event: t.Dict) -> t.Dict:
    return dict(profile=event)


DESCRIPTION = (
    "JobiJoba is part of the HelloWork group, France's leading digital player "
    "in recruitment, employment and training."
)
Jobijoba = Connector(
    name="Jobijoba",
    type=ConnectorType.JobBoard,
    subtype="jobijoba",
    description=DESCRIPTION,
    url="https://www.jobijoba.com/fr/",
    warehouse=JobijobaWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_jobijoba_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_jobijoba_profile,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_jobijoba_profile_for_archive,
        ),
    ),
)
