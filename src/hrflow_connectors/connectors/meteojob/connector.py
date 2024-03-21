import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowProfileParsingWarehouse
from hrflow_connectors.connectors.meteojob.warehouse import MeteojobProfilesWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


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


def format_meteojob_profile(meteojob_profile: t.List) -> t.Dict:
    profile_tags = rename_profile_fields(meteojob_profile)
    tags = add_tags(profile_tags)
    resume_dict = dict(
        raw=meteojob_profile["cv"],
        content_type=meteojob_profile["content_type"],
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
    "METEOJOB : LEADER EUROPÉEN DU MATCHING, BIG DATA ET DES ENTRETIENS VIDÉO DANS"
    " LES RH "
)
Meteojob = Connector(
    name="Meteojob",
    type=ConnectorType.JobBoard,
    description=DESCRIPTION,
    url="https://www.meteojob.com/",
    actions=[
        ConnectorAction(
            name=ActionName.catch_profile,
            trigger_type=WorkflowType.catch,
            description="Imports candidates, in synchronization with Meteojob",
            parameters=BaseActionParameters.with_defaults(
                "TriggerViewActionParameters",
                format=format_meteojob_profile,
                event_parser=event_parser,
            ),
            origin=MeteojobProfilesWarehouse,
            target=HrFlowProfileParsingWarehouse,
            action_type=ActionType.inbound,
        )
    ],
)
