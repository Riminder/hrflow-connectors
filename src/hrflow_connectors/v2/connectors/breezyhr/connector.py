import typing as t

from hrflow_connectors.v2.connectors.breezyhr.utils import (
    is_valid_url,
    remove_html_tags,
)
from hrflow_connectors.v2.connectors.breezyhr.warehouse import BreezyHrWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_location(location: t.Dict) -> t.Dict:
    country = location.get("country", {}).get("name")
    city = location.get("city")
    address = location.get("name")
    street_address = location.get("streetAddress", {}).get("custom")
    geojson = dict(country=country, city=city, text=street_address or address)

    return dict(text=street_address or address, geojson=geojson, lat=None, lng=None)


def format_job(breezy_job: t.Dict) -> t.Dict:
    """
    Format a Breezy Hr job object into a hrflow job object
    Returns:
        HrflowJob: a job object in the hrflow job format
    """
    location = get_location(breezy_job.get("location", {}))
    is_remote = breezy_job.get("location", {}).get("is_remote")
    remote_details = breezy_job.get("location", {}).get("remote_details")
    description = remove_html_tags(breezy_job.get("description", ""))
    cleaned_description = description.replace("&nbsp;", " ")

    sections = [
        dict(
            name="description",
            title="Description",
            description=cleaned_description,
        ),
        dict(
            name="experience",
            title="Required Experience",
            description=breezy_job.get("experience", {}).get("name"),
        ),
        dict(
            name="education",
            title="Required Education",
            description=breezy_job.get("education"),
        ),
        dict(
            name="category",
            title="Category",
            description=breezy_job.get("category", {}).get("name"),
        ),
        dict(
            name="remote",
            title="Remote Information",
            description=remote_details,
        ),
    ]
    t = lambda name, value: dict(name=name, value=value)
    tags = [
        t("type", breezy_job.get("type", {}).get("name")),
        t("experience", breezy_job.get("experience", {}).get("name")),
        t("education", breezy_job.get("education")),
        t("department", breezy_job.get("department")),
        t("requisition_id", breezy_job.get("requisition_id")),
        t("category", breezy_job.get("category", {}).get("name")),
        t("candidate_type", breezy_job.get("candidate_type")),
        t("isremote", is_remote),
        t("remote_details", remote_details),
        t("creator_id", breezy_job.get("creator_id")),
        t("breezy_hr_tags", breezy_job.get("tags")),
    ]

    hrflow_job = dict(
        name=breezy_job.get("name"),
        reference=breezy_job.get("_id"),
        summary=cleaned_description,
        location=location,
        sections=sections,
        tags=tags,
        created_at=breezy_job.get("creation_date"),
        updated_at=breezy_job.get("updated_date"),
    )

    return hrflow_job


def format_archive_in_hrflow(breezy_element: t.Dict) -> t.Dict:
    return dict(reference=breezy_element.get("_id"))


def format_profile(hrflow_profile: t.Dict) -> t.Dict:
    """
    Format a Hrflow profile object into a breezy hr profile object
    Args:
        data (HrflowProfile): Hrflow Profile to format
    Returns:
        BreezyProfileModel: a BreezyHr formatted profile object
    """

    info = hrflow_profile.get("info", {})

    work_history = []
    for experience in hrflow_profile.get("experiences", []):
        formatted_experience = dict()
        if experience.get("company") not in ["", None]:
            formatted_experience["company_name"] = experience.get("company")
        else:
            formatted_experience["company_name"] = "Undefined"
        formatted_experience["title"] = experience.get("title")
        formatted_experience["summary"] = experience.get("description")
        if experience.get("date_start") is not None:
            date_start = experience["date_start"]
            formatted_experience["start_year"] = int(date_start[:4])
            formatted_experience["start_month"] = int(date_start[5:7])
        if experience.get("date_end") is not None:
            date_end = experience["date_end"]
            formatted_experience["end_year"] = int(date_end[:4])
            formatted_experience["end_month"] = int(date_end[5:7])
        work_history.append(formatted_experience)

    educations = []
    for education in hrflow_profile.get("educations", []):
        formatted_education = dict()
        if education.get("school") == "":
            education["school"] = "Undefined"
        formatted_education["school_name"] = education.get("school")
        formatted_education["field_of_study"] = education.get("title")
        if education.get("date_start") is not None:
            date_start = education["date_start"]
            formatted_education["start_year"] = int(date_start[:4])
        if education.get("date_end") is not None:
            date_end = education["date_end"]
            formatted_education["end_year"] = int(date_end[:4])
        educations.append(formatted_education)

    social_profiles = {}
    if info.get("urls"):
        for url in info.get("urls"):
            type = url.get("type")
            link = url.get("url")
            if type and link:
                if type == "from_resume" or not is_valid_url(link):
                    continue
                social_profiles.update({type: link})

    # add profile skills to tags
    tags = []
    skills = hrflow_profile.get("skills")
    if skills:
        tags = [skill.get("name") for skill in skills]

    # add resume to profile
    attachments = hrflow_profile.get("attachments", [])
    resume_url = next(
        (
            attachment
            for attachment in attachments
            if attachment.get("type") == "resume"
        ),
        {},
    ).get("public_url")
    breezy_profile = dict(
        name=info.get("full_name"),
        email_address=info.get("email"),
        phone_number=info.get("phone"),
        address=info.get("location", {}).get("text"),
        summary=info.get("summary"),
        work_history=work_history,
        education=educations,
        social_profiles=social_profiles,
        tags=tags,
        resume=resume_url,
    )
    return breezy_profile


def format_profile_for_update(hrflow_profile: t.Dict) -> t.Dict:
    breezy_profile = format_profile(hrflow_profile)
    breezy_profile["id"] = hrflow_profile.get("reference")
    return breezy_profile


def format_date_to_iso(date):
    year = date.get("year")
    month = date.get("month")
    day = date.get("day")
    # Check if the date is complete, i.e., year, month, and day are all present
    if year is not None and month is not None and day is not None:
        return f"{year:04d}-{month:02d}-{day:02d}"
    elif year is not None and month is not None:
        return f"{year:04d}-{month:02d}"
    elif year is not None:
        return f"{year:04d}"
    else:
        return None


def format_candidate(breezy_profile: t.Dict) -> t.Dict:
    """
    Format a Breezy profile object into a Hrflow profile object
    Args:
        data (BreezyProfileModel): Breezy Profile to format
    Returns:
        HrFlowProfile: a Hrflow formatted profile object
    """
    info = dict(
        full_name=breezy_profile["name"],
        first_name=breezy_profile["name"].split(" ")[0],
        last_name=breezy_profile["name"].split(" ")[-1],
        email=breezy_profile["email_address"],
        phone=breezy_profile["phone_number"],
        urls=[
            dict(type=social_profile["type"], url=social_profile["url"])
            for social_profile in breezy_profile.get("social_profiles", [])
        ],
        summary=breezy_profile["summary"],
        location={"text": breezy_profile.get("address", ""), "lat": None, "lng": None},
    )

    educations = []
    for education in breezy_profile.get("education", []):
        formatted_education = dict()
        formatted_education["school"] = education.get("school_name")
        degree = education.get("degree")
        field_of_study = education.get("field_of_study")
        if degree or field_of_study:
            formatted_education["title"] = (
                (degree if degree else "")
                + " "
                + (field_of_study if field_of_study else "")
            ).strip()
        formatted_education["description"] = (
            formatted_education["title"] + " at " + formatted_education["school"]
        )
        formatted_education["date_start"] = None
        formatted_education["date_end"] = None
        if education.get("start_date") is not None:
            formatted_education["date_start"] = format_date_to_iso(
                education["start_date"]
            )
        if education.get("end_date") is not None:
            formatted_education["date_end"] = format_date_to_iso(education["end_date"])
        formatted_education["location"] = {"text": None, "lat": None, "lng": None}
        educations.append(formatted_education)

    experiences = []
    for experience in breezy_profile.get("work_history", []):
        formatted_experience = dict()
        formatted_experience["company"] = experience.get("company_name")

        formatted_experience["title"] = experience.get("title")
        formatted_experience["description"] = experience.get("summary")
        formatted_experience["date_start"] = None
        formatted_experience["date_end"] = None
        if experience.get("start_date") is not None:
            formatted_experience["date_start"] = format_date_to_iso(
                experience["start_date"]
            )
        if experience.get("end_date") is not None:
            formatted_experience["date_end"] = format_date_to_iso(
                experience["end_date"]
            )
        formatted_experience["location"] = {"text": None, "lat": None, "lng": None}
        experiences.append(formatted_experience)

    tags = []
    t = lambda name, value: dict(name=name, value=value)
    tags.append(t("breezy_hr_tags", breezy_profile.get("tags")))
    tags.append(t("headline", breezy_profile.get("headline")))
    tags.append(t("origin", breezy_profile.get("origin")))
    tags.append(t("source", breezy_profile.get("source", {}).get("name")))
    tags.append(t("sourced_by", breezy_profile.get("sourced_by")))
    tags.append(t("stage", breezy_profile.get("stage", {}).get("name")))
    tags.append(
        t("overall_score", breezy_profile.get("overall_score", {}).get("average_score"))
    )
    hrflow_profile = dict(
        reference=breezy_profile.get("_id"),
        info=info,
        created_at=breezy_profile.get("creation_date"),
        updated_at=breezy_profile.get("updated_date"),
        experiences=experiences,
        educations=educations,
        skills=[],
        tags=tags,
        resume=breezy_profile.get("resume"),
    )

    return hrflow_profile


BreezyHR = Connector(
    name="Breezy HR",
    type=ConnectorType.ATS,
    subtype="breezyhr",
    description=(
        "Breezyhr is an end-to-end recruiting software "
        "to help you attract & hire great employees with less effort"
    ),
    url="https://breezy.hr/",
    warehouse=BreezyHrWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.archive, Entity.job, Direction.inbound, format=format_archive_in_hrflow
        ),
        Flow(Mode.create, Entity.profile, Direction.inbound, format=format_candidate),
        Flow(Mode.update, Entity.profile, Direction.inbound, format=format_candidate),
        Flow(Mode.create, Entity.profile, Direction.outbound, format=format_profile),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=format_profile_for_update,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_archive_in_hrflow,
        ),
    ),
)
