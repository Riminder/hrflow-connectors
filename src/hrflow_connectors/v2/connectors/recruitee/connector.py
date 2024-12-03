import typing as t

from hrflow_connectors.v2.connectors.recruitee.warehouse import RecruiteeWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_profile_cv_url(attachments: t.List[t.Dict]):
    if not attachments:
        return None
    cv_url = next(
        (attachment for attachment in attachments if attachment.get("type") == "resume")
    )["public_url"]
    return cv_url


def get_profile_links(urls: t.List[t.Dict]) -> t.Tuple[t.List[str], t.List[str]]:
    profile_links = []
    social_links = []

    profile_links = [e["url"] for e in urls if e.get("type") == "from_resume"]
    social_links = [e["url"] for e in urls if e.get("type") != "from_resume"]

    return profile_links, social_links


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_profile_info = hrflow_profile["info"]
    profile_links, social_links = get_profile_links(hrflow_profile_info["urls"])
    recruitee_candidate = dict(
        name=hrflow_profile_info["full_name"],
        remote_cv_url=get_profile_cv_url(hrflow_profile["attachments"]),
        emails=[hrflow_profile_info["email"]],
        phones=[hrflow_profile_info["phone"]],
        social_links=social_links,
        links=profile_links,
        cover_letter="",
        sources=[hrflow_profile["source"]["name"]],
    )
    return recruitee_candidate


def format_hrflow_profile_for_update(hrflow_profile: t.Dict) -> t.Dict:
    recruitee_candidate = format_hrflow_profile(hrflow_profile)
    recruitee_candidate["id"] = hrflow_profile["reference"]
    del recruitee_candidate["sources"]
    return recruitee_candidate


def format_archive_in_recruitee(hrflow_profile: t.Dict) -> t.Dict:
    return dict(id=hrflow_profile["reference"])


def get_candidate_tags(recruitee_candidate: t.Dict) -> t.List[t.Dict]:
    tag_keys = [
        "source",
        "sources",
        "positive_ratings",
        "grouped_open_question_answers",
        "cover_letter",
        "last_message_at",
        "is_hired",
    ]
    tags = [{"name": key, "value": recruitee_candidate.get(key)} for key in tag_keys]

    tags += [
        {"name": field.get("name") or field["kind"], "value": field["values"]}
        for field in recruitee_candidate.get("fields", [])
        if field["kind"]
        not in {
            "education",
            "experience",
            "language_skill",
            "skill",
            "date_of_birth",
            "address",
        }
    ]
    tags += [{"name": "tags", "value": recruitee_candidate.get("tags", [])}]
    return tags


def extract_field_values(fields: t.List[t.Dict], kind: str) -> t.List:
    return next((field["values"] for field in fields if field["kind"] == kind), [])


def format_recruitee_candidate(recruitee_candidate: t.Dict) -> t.Dict:
    fields = recruitee_candidate.get("fields", [])

    educations = [
        {
            "school": edu["school"],
            "date_start": edu["start_date"],
            "date_end": edu["end_date"],
            "description": edu["description"],
            "title": edu["major"],
            "location": {"text": "", "lat": None, "lng": None},
        }
        for edu in extract_field_values(fields, "education")
    ]
    experiences = [
        {
            "company": exp["company"],
            "date_start": exp["start_date"],
            "date_end": exp["end_date"],
            "description": exp["description"],
            "title": exp["title"],
            "location": {"text": exp["location"], "lat": None, "lng": None},
        }
        for exp in extract_field_values(fields, "experience")
    ]
    languages = [
        {"name": lang["language_name"], "value": lang["level"]}
        for lang in extract_field_values(fields, "language_skill")
    ]
    skills = [
        {"name": skill["text"], "type": "hard", "value": None}
        for skill in extract_field_values(fields, "skills")
    ]

    address = extract_field_values(fields, "address")
    date_of_birth = extract_field_values(fields, "date_of_birth")
    gender = extract_field_values(fields, "gender")

    # Inline name splitting logic
    full_name = recruitee_candidate.get("name", "")
    name_parts = full_name.split()
    first_name = " ".join(name_parts[:-1]) if len(name_parts) > 1 else full_name
    last_name = name_parts[-1] if len(name_parts) > 1 else ""

    emails = recruitee_candidate.get("emails", [])
    phones = recruitee_candidate.get("phones", [])
    urls = [
        {"url": url, "type": "from_resume"}
        for url in recruitee_candidate.get("social_links", [])
        + recruitee_candidate.get("links", [])
    ]
    profile = {
        "reference": str(recruitee_candidate.get("id")),
        "text": recruitee_candidate.get("description"),
        "info": {
            "full_name": full_name,
            "first_name": first_name,
            "last_name": last_name,
            "email": emails[0] if emails else None,
            "phone": phones[0] if phones else None,
            "date_birth": date_of_birth[0]["date"] if date_of_birth else None,
            "gender": gender[0]["value"] if gender else None,
            "picture": recruitee_candidate.get("photo_url"),
            "urls": urls,
            "location": {
                "text": address[0]["text"] if address else "",
                "lat": None,
                "lng": None,
            },
        },
        "created_at": recruitee_candidate.get("created_at"),
        "updated_at": recruitee_candidate.get("updated_at"),
        "educations": educations,
        "experiences": experiences,
        "languages": languages,
        "skills": skills,
        "resume": {
            "raw": recruitee_candidate.get("cv_file"),
        },
        "tags": get_candidate_tags(recruitee_candidate),
    }
    return profile


def format_archive_in_hrflow(recruitee_candidate: t.Dict) -> t.Dict:
    return dict(reference=str(recruitee_candidate["id"]))


def get_job_tags(recruitee_job: t.Dict) -> t.List[t.Dict]:
    tag_keys = [
        "category",
        "department",
        "options_cv",
        "options_cover_letter",
        "experience",
        "education",
        "employment_type",
        "remote_option",
        "number_of_openings",
        "candidates_count",
        "disqualified_candidates_count",
        "qualified_candidates_count",
        "hired_candidates_count",
        "email_confirmation_body",
        "status",
        "closed_at",
    ]
    tags = [{"name": key, "value": recruitee_job.get(key)} for key in tag_keys]
    tags += [{"name": "offer_tags", "value": recruitee_job.get("offer_tags", [])}]

    return tags


def get_ranges_float(recruitee_job: t.Dict) -> t.List[t.Dict]:
    t = lambda name, value_min, value_max, unit: dict(
        name=name, value_min=value_min, value_max=value_max, unit=unit
    )
    salary = recruitee_job.get("salary", {})
    ranges_float = [
        t(
            "working hours",
            recruitee_job.get("min_hours"),
            recruitee_job.get("max_hours"),
            "Hours per week",
        ),
        t(
            "salary per {}".format(salary.get("period")),
            salary.get("min"),
            salary.get("max"),
            salary.get("currency"),
        ),
    ]
    return ranges_float


def format_job(recruitee_job: t.Dict) -> t.Dict:
    sections = [
        dict(
            name="job_requirements",
            title="Job Requirements",
            description=recruitee_job["requirements"],
        ),
        dict(
            name="job_description",
            title="Job Description",
            description=recruitee_job["description"],
        ),
        dict(
            name="job_highlights",
            title="Job Highlights",
            description=recruitee_job["highlight_html"],
        ),
    ]
    job = dict(
        name=recruitee_job.get("title"),
        reference=str(recruitee_job.get("id")),
        created_at=recruitee_job.get("created_at"),
        updated_at=recruitee_job.get("updated_at"),
        location=dict(
            lat=None,
            lng=None,
            text=recruitee_job.get("location"),
            fields=dict(
                city=recruitee_job.get("city"),
                postcode=recruitee_job.get("postal_code"),
            ),
        ),
        url=recruitee_job.get("url"),
        summary=recruitee_job.get("description"),
        requirements=recruitee_job.get("requirements"),
        sections=sections,
        tags=get_job_tags(recruitee_job),
        ranges_float=get_ranges_float(recruitee_job),
    )
    return job


DESCRIPTION = (
    "Recruitee is an innovative and intuitive recruitment software that empowers"
    " businesses to streamline hiring processes and attract top talent. Recruitee"
    " provides a comprehensive solution that ensures a seamless and efficient"
    " recruitment experience."
)

Recruitee = Connector(
    name="Recruitee",
    type=ConnectorType.ATS,
    subtype="recruitee",
    description=DESCRIPTION,
    url="https://recruitee.com/",
    warehouse=RecruiteeWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_recruitee_candidate,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_recruitee_candidate,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_archive_in_hrflow,
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
            format=format_archive_in_recruitee,
        ),
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.archive, Entity.job, Direction.inbound, format=format_archive_in_hrflow
        ),
    ),
)
