import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowProfileParsingWarehouse
from hrflow_connectors.connectors.jobology.warehouse import JobologyProfilesWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


def rename_profile_fields(jobology_profile: t.Dict) -> t.Dict:
    return {
        "job-key": jobology_profile["jobkey"],
        "first_name": jobology_profile.get("firstName", None),
        "last_name": jobology_profile.get("lastName", None),
        "phone": jobology_profile.get("phone", None),
        "email": jobology_profile.get("email", None),
        "coverText": jobology_profile.get("coverText", None),
        "profile-country": jobology_profile.get("profilecountry", None),
        "profile-regions": jobology_profile.get("profileregions", None),
        "profile-domains": jobology_profile.get("profiledomains", None),
        "job-lien_annonce_site_carriere": jobology_profile.get(
            "joblien_annonce_site_carriere", None
        ),
        "statistic-source": jobology_profile.get("statisticsource", None),
        "statistic-jbsource": jobology_profile.get("statisticjbsource", None),
    }


def add_tags(profile_tags: t.Dict) -> t.Dict:
    tags = []
    for key, value in profile_tags.items():
        if value:
            tags.append({"name": key, "value": value})
    return tags


def format_jobology_profile(jobology_profile: t.List) -> t.Dict:
    profile_tags = rename_profile_fields(jobology_profile)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=jobology_profile["cv"],
        content_type=jobology_profile["content_type"],
    )
    return dict(
        resume=resume_dict,
        reference=jobology_profile["email"],
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
