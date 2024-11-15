import typing as t

from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)
from hrflow_connectors.v1.connectors.hrflow.warehouse import (
    HrFlowProfileParsingWarehouse,
)
from hrflow_connectors.v1.connectors.jobology.warehouse import JobologyProfilesWarehouse


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


def format_jobology_profile(jobology_profile: t.List) -> t.Dict:
    profile_tags = rename_profile_fields(jobology_profile)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=jobology_profile["cv"],
        content_type=jobology_profile["content_type"],
    )
    return dict(
        reference=None,
        resume=resume_dict,
        tags=tags,
        metadatas=[],
        created_at=None,
    )


def event_parser(event: t.Dict) -> t.Dict:
    return dict(profile=event)


DESCRIPTION = (
    "La mission de jobology est de faciliter le processus de recrutement pour les"
    " entreprises "
)
Jobology = Connector(
    name="Jobology",
    type=ConnectorType.JobBoard,
    subtype="jobology",
    description=DESCRIPTION,
    url="https://www.jobology.fr/",
    actions=[
        ConnectorAction(
            name=ActionName.catch_profile,
            trigger_type=WorkflowType.catch,
            description="Imports candidates, in synchronization with jobology",
            parameters=BaseActionParameters.with_defaults(
                "TriggerViewActionParameters",
                format=format_jobology_profile,
                event_parser=event_parser,
            ),
            origin=JobologyProfilesWarehouse,
            target=HrFlowProfileParsingWarehouse,
            action_type=ActionType.inbound,
        )
    ],
)
