import typing as t

from hrflow_connectors.v2.connectors.workable.warehouse import WorkableWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_location(workable_job: t.Dict) -> t.Dict:
    location = workable_job.get("location", {})

    coords = next(
        (
            location["coords"].split(", ")
            for location in workable_job.get("locations", [])
            if location["city"] == location.get("city")
        ),
        None,
    )

    lat = float(coords[0]) if coords is not None else None
    lng = float(coords[1]) if coords is not None else None

    text = location.get("location_str", None)
    if not text:
        text_components = []
        for key in ["city", "region", "zip_code", "country"]:
            if location.get(key):
                text_components.append(location[key])
        text = ", ".join(text_components)

    geojson = dict(
        text=text,
        country=location.get("country"),
        state=location.get("region"),
        city=location.get("city"),
        postcode=location.get("zip_code"),
    )
    return dict(text=text, lat=lat, lng=lng, geojson=geojson)


def format_job(workable_job: t.Dict) -> t.Dict:
    """
    Format a job into the hrflow job object format
    Args:
        data (WorkableJobModel): a job object pulled from workable subdomain
    Returns:
        HrflowJob: a job into the hrflow job object format
    """
    t = lambda name, value: dict(name=name, value=value)

    hrflow_job = dict(
        name=workable_job.get("title"),
        reference=workable_job.get("shortcode"),
        url=workable_job.get("url"),
        location=get_location(workable_job),
        sections=[
            dict(
                name=field_name,
                title=field_name,
                description=workable_job.get(field_name),
            )
            for field_name in [
                "full_description",
                "description",
                "requirements",
                "benefits",
            ]
        ],
        created_at=workable_job.get("created_at"),
        summary=workable_job.get("description"),
        requirements=workable_job.get("requirements"),
        benefits=workable_job.get("benefits"),
        tags=[
            t("id", workable_job.get("id")),
            t("state", workable_job.get("state")),
            t("department", workable_job.get("department")),
            t("application_url", workable_job.get("application_url")),
            t("employment_type", workable_job.get("employment_type")),
            t("industry", workable_job.get("industry")),
            t("function", workable_job.get("function")),
            t("experience", workable_job.get("experience")),
            t("education", workable_job.get("education")),
        ],
        ranges_float=[
            dict(
                name="salary",
                value_min=workable_job.get("salary", {}).get("salary_from"),
                value_max=workable_job.get("salary", {}).get("salary_to"),
                unit=workable_job.get("salary", {}).get("salary_currency"),
            )
        ],
    )

    return hrflow_job


def format_job_archive(
    workable_job: t.Dict,
) -> t.Dict:
    return dict(
        reference=workable_job.get("shortcode"),
    )


def format_candidate(
    workable_candidate: t.Dict,
) -> t.Dict:
    t = lambda name, value: dict(name=name, value=value)

    hrflow_profile = dict(
        reference=workable_candidate.get("id"),
        info=dict(
            full_name=workable_candidate.get("name"),
            first_name=workable_candidate.get("firstname"),
            last_name=workable_candidate.get("lastname"),
            summary=workable_candidate.get("summary"),
            picture=workable_candidate.get("image_url"),
            email=workable_candidate.get("email"),
            phone=workable_candidate.get("phone"),
            location=get_location(workable_candidate),
        ),
        created_at=workable_candidate.get("created_at"),
        updated_at=workable_candidate.get("updated_at"),
        educations=[
            dict(
                school=education.get("school"),
                title=(
                    education.get("degree", "")
                    + " in "
                    + education.get("field_of_study")
                    if education.get("degree") and education.get("field_of_study")
                    else education.get("degree")
                    or education.get("field_of_study")
                    or "Undefined"
                ),
                description=None,
                date_start=education.get("start_date"),
                date_end=education.get("end_date"),
                location=dict(text=None, lat=None, lng=None),
            )
            for education in workable_candidate.get("education_entries", [])
        ],
        experiences=[
            dict(
                company=experience.get("company"),
                title=experience.get("title"),
                date_start=experience.get("start_date"),
                date_end=experience.get("end_date"),
                description=experience.get("summary"),
                location=dict(text=None, lat=None, lng=None),
            )
            for experience in workable_candidate.get("experience_entries", [])
        ],
        skills=[
            dict(name=skill["name"], value=None)
            for skill in workable_candidate.get("skills", [])
        ],
        tags=[
            t("job", workable_candidate.get("job")),
            t("stage", workable_candidate.get("stage")),
            t("disqualified", workable_candidate.get("disqualified")),
            t("disqualified_at", workable_candidate.get("disqualified_at")),
            t("disqualified_reason", workable_candidate.get("disqualified_reason")),
            t("hired_at", workable_candidate.get("hired_at")),
            t("source", workable_candidate.get("source")),
            t("answers", workable_candidate.get("answers")),
            t("workable_tags", workable_candidate.get("tags")),
        ],
        resume=dict(raw=workable_candidate.get("resume")),
    )

    return hrflow_profile


def format_candidate_archive(
    workable_candidate: t.Dict,
) -> t.Dict:
    return dict(
        reference=workable_candidate.get("id"),
    )


def format_profile(
    hrflow_profile: t.Dict,
) -> t.Dict:
    """
    Format a HrflowProfile object into a WorkableCandidate object
    Args:
        data (HrflowProfile): HrflowProfile object
    Returns:
        WorkableCandidate: WorkableCandidate object
    """
    info = hrflow_profile.get("info", {})

    resume_url = next(
        (
            attachment["public_url"]
            for attachment in hrflow_profile.get("attachments", [])
            if attachment["type"] == "resume"
        ),
        None,
    )

    candidate = dict(
        name=info.get("full_name"),
        firstname=info.get("first_name"),
        lastname=info.get("last_name"),
        email=info.get("email"),
        phone=info.get("phone"),
        address=info.get("location", {}).get("text"),
        summary=info.get("summary"),
        image_url=info.get("picture"),
        education_entries=[
            dict(
                school=education.get("school") or "Undefined",
                degree=education.get("title"),
                field_of_study=None,
                start_date=education.get("date_start"),
                end_date=education.get("date_end"),
            )
            for education in hrflow_profile.get("educations", [])
        ],
        experience_entries=[
            dict(
                company=experience.get("company"),
                title=experience.get("title") or "Undefined",
                summary=experience.get("description"),
                start_date=experience.get("date_start"),
                end_date=experience.get("date_end"),
            )
            for experience in hrflow_profile.get("experiences", [])
        ],
        skills=[skill["name"] for skill in hrflow_profile.get("skills", [])],
        social_profiles=[
            dict(
                type=url["type"],
                url=url["url"],
            )
            for url in info.get("urls", [])
        ],
    )

    if resume_url:
        candidate["resume_url"] = resume_url

    return candidate


def fromat_profile_update(
    hrflow_profile: t.Dict,
) -> t.Dict:
    """
    Format a HrflowProfile object into a WorkableCandidate object
    Args:
        data (HrflowProfile): HrflowProfile object
    Returns:
        WorkableCandidate: WorkableCandidate object
    """

    candidate = format_profile(hrflow_profile)
    candidate["id"] = hrflow_profile.get("reference")
    return candidate


Workable = Connector(
    name="Workable",
    type=ConnectorType.HCM,
    subtype="workable",
    warehouse=WorkableWarehouse,
    description=(
        "More than an applicant tracking system, "
        "Workable's talent acquisition software helps teams find candidates, "
        "evaluate applicants and make the right hire, faster."
    ),
    url="https://www.workable.com/",
    flows=(
        Flow(
            Mode.create,
            Entity.job,
            Direction.inbound,
            format=format_job,
        ),
        Flow(
            Mode.update,
            Entity.job,
            Direction.inbound,
            format=format_job,
        ),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=format_job_archive,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_candidate,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_candidate,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_candidate_archive,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=fromat_profile_update,
        ),
    ),
)
