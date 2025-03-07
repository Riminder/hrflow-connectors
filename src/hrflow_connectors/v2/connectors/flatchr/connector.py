import typing as t

from hrflow_connectors.v2.connectors.flatchr.warehouse import FlatchrWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def format_flatchr_profile(flatchr_profile: t.Dict) -> t.Dict:
    hrflow_profile = dict(
        reference=flatchr_profile.get("id"),
        created_at=flatchr_profile.get("created_at"),
        info=dict(
            email=flatchr_profile.get("email"),
            first_name=flatchr_profile.get("firstname"),
            last_name=flatchr_profile.get("lastname"),
            full_name=(
                f"{flatchr_profile.get('firstname')} {flatchr_profile.get('lastname')}"
            ),
            location=dict(text="", lat=None, lng=None),
            phone=flatchr_profile.get("phone"),
        ),
        educations=[],
        experiences=[],
        attachments=[],
        tags=[
            {"name": "applicant_id", "value": flatchr_profile.get("applicant")},
            {"name": "vacancy", "value": flatchr_profile.get("vacancy")},
            {"name": "vacancy_id", "value": flatchr_profile.get("vacancy_id")},
            {"name": "source", "value": flatchr_profile.get("source")},
            {"name": "external_id", "value": flatchr_profile.get("external_id")},
            {"name": "hired", "value": flatchr_profile.get("hired")},
            {"name": "column", "value": flatchr_profile.get("column")},
        ],
        resume=flatchr_profile.get("resume"),
    )
    return hrflow_profile


def format_urls(urls: t.List) -> t.List:
    return [{f"{element['type']}": element["url"]} for element in urls]


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    profile = dict(
        firstname=hrflow_profile["info"]["first_name"],
        lastname=hrflow_profile["info"]["last_name"],
        email=hrflow_profile["info"]["email"],
        phone=hrflow_profile["info"]["phone"],
        urls=format_urls(hrflow_profile["info"]["urls"]),
        type="link",
        resume=next(
            (
                attachment
                for attachment in hrflow_profile["attachments"]
                if attachment.get("type") == "resume"
            ),
            {},
        ).get("public_url"),
        source=f"HrFlow Source: {hrflow_profile['source']['name']}",
    )
    return profile


def format_hrflow_profile_for_update(hrflow_profile: t.Dict) -> t.Dict:
    flatchr_profile = format_hrflow_profile(hrflow_profile)
    flatchr_profile["id"] = hrflow_profile["reference"]
    return flatchr_profile


def format_flatchr_profile_for_archive(flatchr_profile: t.Dict) -> t.Dict:
    return {"reference": flatchr_profile["id"]}


def format_hrflow_profile_for_archive(hrflow_profile: t.Dict) -> t.Dict:
    vacancy_id = next(
        (tag["value"] for tag in hrflow_profile["tags"] if tag["name"] == "vacancy_id"),
        None,
    )
    applicant_id = next(
        (
            tag["value"]
            for tag in hrflow_profile["tags"]
            if tag["name"] == "applicant_id"
        ),
        None,
    )
    return {"applicant_id": applicant_id, "vacancy_id": vacancy_id}


def get_tags(flatchr_job: t.Dict) -> t.List:
    t = lambda name, value: dict(name=name, value=value)
    tags = [
        t("slug", flatchr_job.get("slug")),
        t("status", flatchr_job.get("status")),
        t("language", flatchr_job.get("language")),
        t("company_id", flatchr_job.get("company_id")),
        t("currency", flatchr_job.get("currency")),
        t("mensulaity", flatchr_job.get("mensulaity")),
        t("start_date", flatchr_job.get("start_date")),
        t("end_date", flatchr_job.get("end_date")),
        t("driver_license", flatchr_job.get("driver_license")),
        t("remote", flatchr_job.get("remote")),
        t("handicap", flatchr_job.get("handicap")),
        t("partial", flatchr_job.get("partial")),
    ]
    return tags


def format_flatchr_job(flatchr_job: t.Dict) -> t.Dict:
    hrflow_job = dict(
        reference=flatchr_job.get("id"),
        name=flatchr_job.get("title"),
        created_at=flatchr_job.get("created_at"),
        updated_at=flatchr_job.get("updated_at"),
        summary=flatchr_job.get("description"),
        location=dict(
            text="",
            lat=None,
            lng=None,
        ),
        sections=[
            dict(
                name="description",
                title="Description",
                description=flatchr_job.get("description"),
            ),
            dict(
                name="mission", title="Mission", description=flatchr_job.get("mission")
            ),
            dict(
                name="profile", title="Profile", description=flatchr_job.get("profile")
            ),
        ],
        responsibilities=flatchr_job.get("mission"),
        requirements=flatchr_job.get("profile"),
        url=flatchr_job.get("apply_url"),
        skills=[
            dict(name=skill, value=None, type="soft")
            for skill in (flatchr_job.get("skills") or "").split(";")
        ],
        tags=get_tags(flatchr_job),
    )
    return hrflow_job


def format_flatchr_job_for_archive(flatchr_job: t.Dict) -> t.Dict:
    return {"reference": flatchr_job["id"]}


DESCRIPTION = (
    "Avec le digital, recruter est devenu beaucoup plus compliqué qu'avant."
    " Flatchr centralise et simplifie vos processus de recrutement pour le rendre aussi"
    " simple que télécharger un film sur Netflix."
)
Flatchr = Connector(
    name="Flatchr",
    type=ConnectorType.ATS,
    subtype="flatchr",
    description=DESCRIPTION,
    url="https://www.flatchr.io/",
    warehouse=FlatchrWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_flatchr_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_flatchr_profile,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_flatchr_profile_for_archive,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_for_update,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_for_archive,
        ),
        Flow(
            Mode.create,
            Entity.job,
            Direction.inbound,
            format=format_flatchr_job,
        ),
        Flow(
            Mode.update,
            Entity.job,
            Direction.inbound,
            format=format_flatchr_job,
        ),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=format_flatchr_job_for_archive,
        ),
    ),
)
