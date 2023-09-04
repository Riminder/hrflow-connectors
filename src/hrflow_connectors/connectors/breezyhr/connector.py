import re

from hrflow_connectors.connectors.breezyhr.utils.datetime_converter import (
    from_str_to_datetime,
)
from hrflow_connectors.connectors.breezyhr.utils.remove_html_tags import (
    remove_html_tags,
)
from hrflow_connectors.connectors.breezyhr.warehouse import (
    BreezyHRJobWarehouse,
    BreezyHRProfileWarehouse,
)
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.core.connector import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)

from ..hrflow.schemas import HrFlowJob, HrFlowProfile
from .schemas import BreezyJobModel, BreezyProfileModel


def format_jobs(breezy_job: BreezyJobModel) -> HrFlowJob:
    """
    Format a Breezy Hr job object into a hrflow job object
    Returns:
        HrflowJob: a job object in the hrflow job format
    """
    hrflow_job = dict()
    # Basic information
    hrflow_job["name"] = breezy_job.get("name")
    hrflow_job["reference"] = breezy_job.get("friendly_id")
    hrflow_job["summary"] = None

    # Location
    location = breezy_job.get("location")
    country = location.get("country")
    country_name = country.get("name")
    city = location.get("city")
    address = location.get("name")
    geojson = dict(country=country_name, city=city)

    hrflow_job["location"] = dict(text=address, geojson=geojson, lat=None, lng=None)

    # Sections
    description = remove_html_tags(breezy_job.get("description"))
    cleaned_description = description.replace("&nbsp;", " ")
    hrflow_job["sections"] = [
        dict(
            name="breezy_hr_description",
            title="Breezy_hr_description",
            description=cleaned_description,
        )
    ]
    # tags
    hrflow_job["tags"] = []

    def create_tag(field_name: str):
        tag_name = f"breezy_hr_{field_name}"
        tag_value = breezy_job.get(field_name)

        if isinstance(tag_value, dict):
            tag_name_value = tag_value.get("name")
            tag = dict(name=tag_name, value=tag_name_value)
            hrflow_job["tags"].append(tag)
        if isinstance(tag_value, str):
            tag = dict(name=tag_name, value=tag_value)
            hrflow_job["tags"].append(tag)

    create_tag("type")
    create_tag("experience")
    create_tag("education")
    create_tag("department")
    create_tag("requisition_id")
    create_tag("category")
    create_tag("candidate_type")
    is_remote = dict(name="breezy_hr_remote", value=location.get("is_remote"))
    hrflow_job["tags"].append(is_remote)

    hrflow_job["created_at"] = breezy_job.get("creation_date")
    hrflow_job["updated_at"] = breezy_job.get("updated_date")

    return hrflow_job


def format_profile(hrflow_profile: HrFlowProfile) -> BreezyProfileModel:
    """
    Format a Hrflow profile object into a breezy hr profile object
    Args:
        data (HrflowProfile): Hrflow Profile to format
    Returns:
        BreezyProfileModel: a BreezyHr formatted profile object
    """

    breezy_profile = dict()
    info = hrflow_profile.get("info")
    breezy_profile["name"] = info.get("full_name")
    breezy_profile["address"] = info.get("location").get("text")
    breezy_profile["email_address"] = info.get("email")
    breezy_profile["phone_number"] = info.get("phone")
    breezy_profile["summary"] = info.get("summary")
    breezy_profile["work_history"] = []

    def format_experiences():
        experiences = hrflow_profile.get("experiences")
        for experience in experiences:
            format_experience = dict()
            if experience.get("company") not in ["", None]:
                format_experience["company_name"] = experience.get("company")
            else:
                format_experience["company_name"] = "Undefined"
            format_experience["title"] = experience.get("title")
            format_experience["summary"] = experience.get("description")
            if experience.get("date_start") is not None:
                date_iso = from_str_to_datetime((experience.get("date_start")))
                format_experience["start_year"] = date_iso.year
                format_experience["start_month"] = date_iso.month
            if experience.get("date_end") is not None:
                date_end_iso = from_str_to_datetime((experience.get("date_end")))
                format_experience["end_year"] = date_end_iso.year
                format_experience["end_month"] = date_end_iso.month

            breezy_profile["work_history"].append(format_experience)

    format_experiences()

    breezy_profile["education"] = []

    def format_educations():
        educations = hrflow_profile.get("educations")
        for education in educations:
            format_education = dict()
            if education.get("school") == "":
                education["school"] = "Undefined"
            format_education["school_name"] = education.get("school")
            format_education["field_of_study"] = education.get("title")
            if education.get("date_start") is not None:
                date_iso = from_str_to_datetime((education.get("date_start")))
                format_education["start_year"] = date_iso.year
            if education.get("date_end") is not None:
                date_end_iso = from_str_to_datetime((education.get("date_end")))
                format_education["end_year"] = date_end_iso.year
            breezy_profile["education"].append(format_education)

    format_educations()

    breezy_profile["social_profiles"] = {}

    def format_urls() -> None:
        """
        format_urls, add links and websites to Taleez profile Social links
        """
        urls = info.get("urls")
        if isinstance(urls, list):
            for url in urls:
                try:
                    type = url.get("type")
                    link = url.get("url")
                    if isinstance(link, str):
                        if not re.match(r"http(s)?:\/\/.*", link):
                            # To bypass Breezy invalid_url when is valid_url
                            link = "https://" + link
                        breezy_profile.get("social_profiles").update({type: link})
                except Exception:
                    continue
        attachments = info.get("attachments")
        if isinstance(attachments, list):
            for attachment in attachments:
                file_name = attachment.get("file_name")
                public_url = attachment.get("public_url")
                if isinstance(public_url, str):
                    breezy_profile.get("social_profiles").update(
                        {file_name: public_url}
                    )

    format_urls()

    # add profile skills to tags
    breezy_profile["tags"] = []
    skills = hrflow_profile.get("skills")
    if isinstance(skills, list):
        for skill in skills:
            if isinstance(skill, dict):
                breezy_profile["tags"].append(skill.get("name"))

    return breezy_profile


BreezyHR = Connector(
    name="BreezyHR",
    type=ConnectorType.ATS,
    description=(
        "Breezyhr is an end-to-end recruiting software "
        "to help you attract & hire great employees with less effort"
    ),
    url="https://breezy.hr/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the ***BreezyHR*** API and send them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "PullJobsActionParameters", format=format_jobs
            ),
            origin=BreezyHRJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile_list,
            trigger_type=WorkflowType.catch,
            description=(
                "Push all profiles from ***Hrflow.ai Source*** via ***BreezyHR*** API"
                " and send them to a ***BreezyHR***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "PushProfilesActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=BreezyHRProfileWarehouse,
            action_type=ActionType.outbound,
        ),
    ],
)
