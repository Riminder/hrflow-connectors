import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowProfileParsingWarehouse
from hrflow_connectors.connectors.jobijoba.warehouse import JobijobaProfilesWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


def rename_profile_fields(jobijoba_profile: t.Dict) -> t.Dict:
    job = jobijoba_profile.get("job")
    profile = jobijoba_profile.get("applicant")
    return {
        "job-number": job.get("jobId", "")[:10] or None,
        "first_name": profile.get("firstName"),
        "last_name": profile.get("lastName"),
        "phone": profile.get("phoneNumber"),
        "email": profile.get("email"),
        "job-lien_annonce_site_carriere": jobijoba_profile.get("jobAtsUrl"),
        "statistic-source": jobijoba_profile.get("source"),
        "statistic-jbsource": jobijoba_profile.get("source"),
    }


def add_tags(profile_tags: t.Dict) -> t.List[t.Dict]:
    return [dict(name=key, value=value) for key, value in profile_tags.items() if value]


def format_jobijoba_profile(jobijoba_profile: t.List) -> t.Dict:
    profile_tags = rename_profile_fields(jobijoba_profile)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=jobijoba_profile["cv"],
        content_type=jobijoba_profile["content_type"],
    )
    return dict(
        reference=None,
        created_at=None,
        resume=resume_dict,
        tags=tags,
        metadatas=[],
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
    description=DESCRIPTION,
    url="https://www.jobijoba.com/fr/",
    actions=[
        ConnectorAction(
            name=ActionName.catch_profile,
            trigger_type=WorkflowType.catch,
            description="Imports candidates, in synchronization with Jobijoba",
            parameters=BaseActionParameters.with_defaults(
                "TriggerViewActionParameters",
                format=format_jobijoba_profile,
                event_parser=event_parser,
            ),
            origin=JobijobaProfilesWarehouse,
            target=HrFlowProfileParsingWarehouse,
            action_type=ActionType.inbound,
        )
    ],
)
