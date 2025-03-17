import typing as t
from datetime import datetime

from hrflow_connectors.v2.connectors.recruiterflow.warehouse import (
    RecruiterFlowWarehouse,
)
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def format_date_to_iso(date_list: t.List[str | None]) -> t.Optional[str]:
    month, year = date_list
    if not month or not year:
        return None
    return f"{year:04d}-{month:02d}"


def format_rf_profile(rf_profile: t.Dict) -> t.Dict:
    t = lambda name, value: dict(name=name, value=value)
    hrflow_profile = dict(
        reference=str(rf_profile["id"]),
        created_at=rf_profile["added_time"],
        info=dict(
            first_name=rf_profile["first_name"],
            last_name=rf_profile["last_name"],
            full_name=rf_profile["name"],
            email=rf_profile["email"][0] if rf_profile["email"] else None,
            phone=rf_profile["phone_number"][0] if rf_profile["phone_number"] else None,
            picture=rf_profile["img_link"],
            location=dict(
                text=rf_profile["location"]["location"],
                lat=None,
                lng=None,
                fields=dict(
                    city=rf_profile["location"]["city"],
                    state=rf_profile["location"]["state"],
                    country=rf_profile["location"]["country"],
                    postcode=rf_profile["location"]["postal_code"],
                ),
            ),
            urls=[
                dict(linkedin=rf_profile["linkedin_profile"]),
                dict(angellist=rf_profile["angellist_profile"]),
                dict(github=rf_profile["github_profile"]),
                dict(facebook=rf_profile["facebook_profile"]),
                dict(twitter=rf_profile["twitter_profile"]),
                dict(xing=rf_profile["xing_profile"]),
                dict(behance=rf_profile["behance_profile"]),
                dict(dribbble=rf_profile["dribbble_profile"]),
            ],
            summary=rf_profile["candidate_summary"],
        ),
        educations=[
            dict(
                school=edu["school"],
                title=edu["degree"],
                description=edu["specialization"],
                date_start=format_date_to_iso(edu["from"]),
                date_end=format_date_to_iso(edu["to"]),
                location=dict(text=None, lat=None, lng=None),
            )
            for edu in rf_profile["education"]
        ],
        experiences=[
            dict(
                title=exp["designation"],
                company=exp["organization"],
                date_start=format_date_to_iso(exp["from"]),
                date_end=format_date_to_iso(exp["to"]),
                description=exp["description"] or "",
                location=dict(text=None, lat=None, lng=None),
            )
            for exp in rf_profile["experience"]
        ],
        skills=[
            dict(name=skill, value=None, type=None) for skill in rf_profile["skills"]
        ],
        tags=[
            t("added_by", rf_profile["added_by"]["name"]),
            t("source_name", rf_profile["source_name"]),
            t("client", rf_profile["client"]["name"] if rf_profile["client"] else None),
            t("status", rf_profile["status"]["name"]),
            t("current_designation", rf_profile["current_designation"]),
            t("current_organization", rf_profile["current_organization"]),
            t("do_not_email", rf_profile["do_not_email"]),
            t("rating", rf_profile["rating"]["name"]),
        ],
        resume=dict(raw=rf_profile["resume"]),
    )
    return hrflow_profile


def rf_object_for_archive(rf_object: t.Dict) -> t.Dict:
    return dict(
        reference=str(
            rf_object["id"],
        )
    )


def iso_to_month_year(date_str: t.Optional[str]) -> t.List[int | None]:
    if not date_str:
        return [None, None]
    date = datetime.fromisoformat(date_str)
    return [date.month, date.year]


def find_url(urls: t.Dict, key: str) -> t.Optional[str]:
    return next((url["url"] for url in urls if url["type"] == key), None)


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    rf_candidate = dict(
        name=hrflow_profile["info"]["full_name"],
        first_name=hrflow_profile["info"]["first_name"],
        last_name=hrflow_profile["info"]["last_name"],
        email=[{"email": hrflow_profile["info"]["email"], "is_primary": 1}],
        phone_number=[{"phone_number": hrflow_profile["info"]["phone"], "type": 1}],
        linkedin_profile=find_url(hrflow_profile["info"]["urls"], "linkedin"),
        twitter_profile=find_url(hrflow_profile["info"]["urls"], "twitter"),
        facebook_profile=find_url(hrflow_profile["info"]["urls"], "facebook"),
        github_profile=find_url(hrflow_profile["info"]["urls"], "github"),
        skills=[skill["name"] for skill in hrflow_profile["skills"]],
        title=hrflow_profile["info"]["summary"],
        source=hrflow_profile["source"]["name"],
        location=dict(
            location=hrflow_profile["info"]["location"]["text"],
            city=hrflow_profile["info"]["location"]["fields"].get("city"),
            state=hrflow_profile["info"]["location"]["fields"].get("state"),
            country=hrflow_profile["info"]["location"]["fields"].get("country"),
            postal_code=hrflow_profile["info"]["location"]["fields"].get("postcode"),
        ),
        experience=[
            {
                "organization": exp["company"] or "Undefined",
                "designation": exp["title"],
                "from": iso_to_month_year(exp["date_start"]),
                "to": iso_to_month_year(exp["date_end"]),
            }
            for exp in hrflow_profile["experiences"]
        ],
        education=[
            {
                "school": edu["school"] or "Undefined",
                "degree": edu["title"],
                "specialization": edu["description"],
                "from": iso_to_month_year(edu["date_start"]),
                "to": iso_to_month_year(edu["date_end"]),
            }
            for edu in hrflow_profile["educations"]
        ],
        added_time=hrflow_profile["created_at"],
        resume=hrflow_profile["attachments"][0],
    )
    return rf_candidate


def format_hrflow_profile_for_update(hrflow_profile: t.Dict) -> t.Dict:
    rf_candidate = format_hrflow_profile(hrflow_profile)
    rf_candidate["id"] = hrflow_profile["reference"]
    return rf_candidate


def format_rf_job(rf_job: t.Dict) -> t.Dict:
    t = lambda name, value: dict(name=name, value=value)
    hrflow_job = dict(
        reference=str(rf_job["id"]),
        name=rf_job["title"] or rf_job["name"],
        location=dict(
            text=rf_job["locations"][0]["name"] if rf_job["locations"] else None,
            lat=None,
            lng=None,
            fields=dict(
                city=rf_job["locations"][0]["city"] if rf_job["locations"] else None,
                state=rf_job["locations"][0]["state"] if rf_job["locations"] else None,
                country=(
                    rf_job["locations"][0]["country"] if rf_job["locations"] else None
                ),
                postcode=(
                    rf_job["locations"][0]["postal_code"]
                    if rf_job["locations"]
                    else None
                ),
            ),
        ),
        summary=rf_job.get("about_position"),
        sections=[
            dict(
                name="job_description",
                value=rf_job.get("about_position"),
            ),
        ],
        url=rf_job.get("apply_link"),
        ranges_float=[
            dict(
                name="experience_range",
                value_min=rf_job["experience_range_start"],
                value_max=rf_job["experience_range_end"],
                unit="year",
            ),
            dict(
                name=f"salary_per_{rf_job['salary_frequency']}",
                value_min=rf_job["salary_range_start"],
                value_max=rf_job["salary_range_end"],
                unit=rf_job["salary_range_currency"],
            ),
        ],
        ranges_date=[
            dict(
                name="contract_period",
                value_min=rf_job["contract_start_date"],
                value_max=rf_job["contract_end_date"],
            )
        ],
        tags=[
            t("company", rf_job["company"]["name"]),
            t("current_opening", rf_job["current_opening"]),
            t("department", rf_job["department"]),
            t("employment_type", rf_job["employment_type"]),
            t("is_open", rf_job["is_open"]),
            t("is_full_time", rf_job.get("work_quantum", {}).get("is_full_time")),
            t("job_status", rf_job["job_status"]["name"]),
            t("job_type", rf_job["job_type"]["name"]),
            t("number_of_openings", rf_job["number_of_openings"]),
        ],
    )
    return hrflow_job


Recruiterflow = Connector(
    name="Recruiterflow",
    type=ConnectorType.ATS,
    subtype="recruiterflow",
    description=(
        "CRM For Recruitment Agencies — Recruiterflow is the complete ecosystem that"
        " helps you run and scale. Purpose built for recruiting agencies."
    ),
    url="https://recruiterflow.com/",
    warehouse=RecruiterFlowWarehouse,
    flows=(
        Flow(Mode.create, Entity.profile, Direction.inbound, format=format_rf_profile),
        Flow(Mode.update, Entity.profile, Direction.inbound, format=format_rf_profile),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=rf_object_for_archive,
        ),
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_rf_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_rf_job),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=rf_object_for_archive,
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
    ),
)
