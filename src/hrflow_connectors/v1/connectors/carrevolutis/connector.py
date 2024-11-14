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
from hrflow_connectors.v1.connectors.carrevolutis.warehouse import (
    CarrevolutisProfilesWarehouse,
)
from hrflow_connectors.v1.connectors.hrflow.warehouse import (
    HrFlowProfileParsingWarehouse,
)


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


def format_carrevolutis_profile(carrevolutis_profile: t.List) -> t.Dict:
    profile_tags = rename_profile_fields(carrevolutis_profile)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=carrevolutis_profile["cv"],
        content_type=carrevolutis_profile["content_type"],
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
    "L'activité principale de Carrevolutis est le recrutement en France, "
    "avec des solutions informatiques et des conseils financiers."
)
Carrevolutis = Connector(
    name="Carrevolutis",
    type=ConnectorType.JobBoard,
    subtype="carrevolutis",
    description=DESCRIPTION,
    url="https://www.carrevolutis.com/",
    actions=[
        ConnectorAction(
            name=ActionName.catch_profile,
            trigger_type=WorkflowType.catch,
            description="Imports candidates, in synchronization with Carrevolutis",
            parameters=BaseActionParameters.with_defaults(
                "TriggerViewActionParameters",
                format=format_carrevolutis_profile,
                event_parser=event_parser,
            ),
            origin=CarrevolutisProfilesWarehouse,
            target=HrFlowProfileParsingWarehouse,
            action_type=ActionType.inbound,
        )
    ],
)
