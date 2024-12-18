import typing as t

from hrflow_connectors.v2.connectors.teamtailor.warehouse import TeamTailorWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def format_job(teamtailor_job: t.Dict) -> t.Dict:
    """
    Format a Teamtailor job object into a Hrflow job object
    Args:
        teamtailor_job: Teamtailor job object from the list of jobs pulled
    Returns:
        HrflowJob: a job in the HrFlow job object form
    """
    job_attributes = teamtailor_job["attributes"]
    t = lambda name, value: dict(name=name, value=value)
    job = dict(
        reference=teamtailor_job.get("id"),
        name=job_attributes.get("title"),
        summary=job_attributes.get("body"),
        created_at=job_attributes.get("created-at"),
        updated_at=job_attributes.get("updated-at"),
        url=teamtailor_job.get("links", {}).get("careersite-job-url"),
        location=teamtailor_job.get("location"),
        sections=[
            dict(
                name="job_description",
                title="Job Description",
                description=job_attributes.get("body"),
            ),
            dict(
                name="job_pitch",
                title="Job Pitch",
                description=job_attributes.get("pitch"),
            ),
        ],
        tags=[
            t("start-date", job_attributes.get("start-date")),
            t("end-date", job_attributes.get("end-date")),
            t("status", job_attributes.get("status")),
            t("human-status", job_attributes.get("human-status")),
            t("language-code", job_attributes.get("language-code")),
            t("employment-type", job_attributes.get("employment-type")),
            t("employment-level", job_attributes.get("employment-level")),
            t("remote-status", job_attributes.get("remote-status")),
            t("salary-time-unit", job_attributes.get("salary-time-unit")),
            t("min-salary", job_attributes.get("min-salary")),
            t("max-salary", job_attributes.get("max-salary")),
            t("currency", job_attributes.get("currency")),
            t("internal", job_attributes.get("internal")),
            t("internal-name", job_attributes.get("internal-name")),
            t("pinned", job_attributes.get("pinned")),
            t("teamtailor_tags", job_attributes.get("tags")),
            t(
                "careersite-job-apply-url",
                teamtailor_job.get("links", {}).get("careersite-job-apply-url"),
            ),
        ],
        ranges_float=[
            dict(
                name="Salary",
                value_min=job_attributes.get("min-salary"),
                value_max=job_attributes.get("max-salary"),
                unit=job_attributes.get("currency"),
            ),
        ],
        ranges_date=[
            dict(
                name="Period",
                value_min=job_attributes.get("start-date"),
                value_max=job_attributes.get("end-date"),
            ),
        ],
    )

    return job


def format_teamtailor_item_for_archive(item: t.Dict) -> t.Dict:
    """
    Format a Teamtailor object to be archived in Hrflow
    Args:
        item (Dict): Teamtailor object (profile/job)
    Returns:
        Dict: reference to an Hrflow object to be archived
    """
    return {"reference": item.get("id")}


def format_teamtailor_profile(teamtailor_profile: t.Dict) -> t.Dict:
    """
    Format a Teamtailor profile object into a Hrflow profile object
    Args:
        teamtailor_profile: Teamtailor Profile to format
    Returns:
        Dict[str, Any]: a Hrflow formatted profile object
    """
    profile_attributes = teamtailor_profile["attributes"]
    t = lambda name, value: dict(name=name, value=value)
    profile = dict(
        reference=teamtailor_profile.get("id"),
        info=dict(
            first_name=profile_attributes.get("first-name"),
            last_name=profile_attributes.get("last-name"),
            full_name=(
                f"{profile_attributes.get('first-name')} "
                f"{profile_attributes.get('last-name')}"
            ),
            email=profile_attributes.get("email"),
            phone=profile_attributes.get("phone"),
            location=dict(text="", lat=None, lng=None),
            picture=profile_attributes.get("picture"),
            urls=[dict(type="linkedin", url=profile_attributes.get("linkedin-url"))],
            summary=profile_attributes.get("pitch"),
        ),
        updated_at=profile_attributes.get("updated-at"),
        created_at=profile_attributes.get("created-at"),
        experiences=[],
        educations=[],
        resume=teamtailor_profile.get("resume"),
        tags=[
            t("sourced", profile_attributes.get("sourced")),
            t("connected", profile_attributes.get("connected")),
            t("internal", profile_attributes.get("internal")),
            t("referred", profile_attributes.get("referred")),
            t("referring_url", profile_attributes.get("referring-url")),
            t("referring_site", profile_attributes.get("referring-site")),
            t("unsubscribed", profile_attributes.get("unsubscribed")),
            t("teamtailor_tags", profile_attributes.get("tags")),
        ],
    )
    return profile


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    """
    Format a Hrflow profile object into a Teamtailor profile object
    Args:
        hrflow_profile: Hrflow Profile to format
    Returns:
        Dict[str, Any]: a Teamtailor formatted profile object
    """
    profile_info = hrflow_profile["info"]
    profile = {
        "first-name": profile_info.get("first_name"),
        "last-name": profile_info.get("last_name"),
        "email": profile_info.get("email"),
        "phone": profile_info.get("phone"),
        "picture": profile_info.get("picture"),
        "linkedin-url": next(
            (
                url["url"]
                for url in profile_info.get("urls", [])
                if url["type"] == "linkedin"
            ),
            None,
        ),
        "pitch": profile_info.get("summary"),
        "resume": (
            hrflow_profile["attachments"][0].get("public_url")
            if hrflow_profile["attachments"]
            else None
        ),
        "sourced": True,
    }
    return profile


def format_hrflow_profile_for_update(hrflow_profile: t.Dict) -> t.Dict:
    """
    Format a Hrflow profile object into a Teamtailor profile object for update
    Args:
        hrflow_profile: Hrflow Profile to format
    Returns:
        Dict[str, Any]: a Teamtailor formatted profile object
    """
    profile = format_hrflow_profile(hrflow_profile)
    profile["id"] = hrflow_profile["reference"]
    return profile


DESCRIPTION = (
    "The new way to attract, nurture and hire top talent. Grow faster by focusing on"
    " what matters the most — your candidates"
)

Teamtailor = Connector(
    name="Teamtailor",
    type=ConnectorType.ATS,
    subtype="teamtailor",
    description=DESCRIPTION,
    url="https://www.teamtailor.com/",
    warehouse=TeamTailorWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=format_teamtailor_item_for_archive,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_teamtailor_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_teamtailor_profile,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_teamtailor_item_for_archive,
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
