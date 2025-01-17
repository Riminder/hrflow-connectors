import typing as t

from hrflow_connectors.v2.connectors.monster.warehouse import MonsterWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_profile_location(monster_profile_location: t.Dict) -> t.Dict:
    concatenate = []
    for field in ["postalCode", "city", "state", "country"]:
        if monster_profile_location.get(field):
            concatenate.append(monster_profile_location[field])

    location = dict(
        text=" ".join(concatenate),
        lat=None,
        lng=None,
        fields=dict(
            city=monster_profile_location.get("city"),
            country=monster_profile_location.get("country"),
            state=monster_profile_location.get("state"),
            postcode=monster_profile_location.get("postalCode"),
        ),
    )
    return location


def get_profile_tags(monster_profile: t.Dict) -> t.List[t.Dict]:
    t = lambda name, value: dict(name=name, value=value)
    tags = [
        t("textResumeID", monster_profile.get("identity", {}).get("textResumeID")),
        t("resumeTitle", monster_profile.get("resumeTitle")),
        t("targetJobTitle", monster_profile.get("targetJobTitle")),
        t("highestEducationDegree", monster_profile.get("highestEducationDegree")),
        t("veteran", monster_profile.get("veteran")),
        t("lastActive", monster_profile.get("lastActive")),
        t("willRelocate", monster_profile.get("location", {}).get("willRelocate")),
        t("willTravel", monster_profile.get("willTravel")),
        t("relevanceScore", monster_profile.get("relevance", {}).get("score")),
        t(
            "securityClearance",
            monster_profile.get("securityClearance", {}).get("clearance"),
        ),
        t("source", monster_profile.get("source")),
        *[
            t("workAuthorization", wa.get("authorization"))
            for wa in monster_profile.get("location", {}).get("workAuthorizations", [])
        ],
        *[t("board", board.get("name")) for board in monster_profile.get("boards", [])],
    ]
    return tags


def format_profile(monster_profile: t.Dict) -> t.Dict:
    """
    Format a Monster profile to the HRflow standard profile format
    """
    monster_profile_identity = monster_profile.get("identity", {})
    hrflow_profile = dict(
        reference=monster_profile_identity.get("seekerRefCode"),
        info=dict(
            full_name=monster_profile_identity.get("name"),
            first_name=monster_profile_identity.get("name").split(" ")[0],
            last_name=monster_profile_identity.get("name").split(" ")[-1],
            email=monster_profile_identity.get("emailAddress"),
            phone=monster_profile.get("phoneNumbers", [{}])[0].get("phoneNumberValue"),
            location=get_profile_location(monster_profile.get("location", {})),
        ),
        experiences_duration=monster_profile.get("yearsOfExperience"),
        experiences=[
            dict(
                company=monster_profile.get("relevance", {})
                .get("experience", {})
                .get("company", {})
                .get("name"),
                title=monster_profile.get("relevance", {})
                .get("experience", {})
                .get("title", {})
                .get("name"),
                date_start=monster_profile.get("relevance", {})
                .get("experience", {})
                .get("start"),
                date_end=monster_profile.get("relevance", {})
                .get("experience", {})
                .get("end"),
                description=None,
                location=dict(text=None, lat=None, lng=None),
            )
        ],
        educations=[
            dict(
                title=education.get("degree"),
                school=education.get("schoolName"),
                date_start=education.get("start"),
                date_end=education.get("end"),
                description=None,
                location=dict(text=None, lat=None, lng=None),
            )
            for education in monster_profile.get("educationalHistory", [])
        ],
        skills=[
            dict(
                name=skill.get("name"),
                type=None,
                value=None,
            )
            for skill in monster_profile.get("relevance", {}).get("skills", [])
        ],
        tags=get_profile_tags(monster_profile),
        resume=dict(
            file_name=monster_profile.get("resumeDocument", {}).get("fileName"),
            content_type=monster_profile.get("resumeDocument", {}).get(
                "fileContentType"
            ),
            raw=monster_profile.get("resumeDocument", {}).get("file"),
        ),
    )
    return hrflow_profile


def format_profile_for_archive(monster_profile: t.Dict) -> t.Dict:
    return dict(reference=monster_profile.get("identity", {}).get("seekerRefCode"))


DESCRIPTION = (
    "Monster is a career development and job search platform. It offers a wide range of"
    " services for job seekers, including resume and cover letter building tools, job"
    " search features, career advice, and more. Additionally, Monster provides"
    " recruiting and hiring solutions for employers, such as job posting, candidate"
    " screening, and applicant tracking tools. The platform is available in more than"
    " 40 countries and is available in multiple languages."
)

Monster = Connector(
    name="Monster",
    type=ConnectorType.JobBoard,
    subtype="monster",
    description=DESCRIPTION,
    url="https://www.monster.com/",
    warehouse=MonsterWarehouse,
    flows=(
        Flow(Mode.create, Entity.profile, Direction.inbound, format=format_profile),
        Flow(Mode.update, Entity.profile, Direction.inbound, format=format_profile),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_profile_for_archive,
        ),
    ),
)
