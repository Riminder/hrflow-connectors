import re

from hrflow_connectors.connectors.breezyhr.warehouse import (
    BreezyHRJobWarehouse,
    BreezyHRProfileWarehouse,
)
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.core.connector import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
from hrflow_connectors.utils.datetime_converter import from_str_to_datetime
from hrflow_connectors.utils.remove_html_tags import remove_html_tags

from ..hrflow.schemas import HrFlowJob, HrFlowProfile
from .schemas import BreezyJobModel, BreezyProfileModel


def format_jobs(breezy_jobs: BreezyJobModel) -> HrFlowJob:
    """
    Format a Breezy Hr job object into a hrflow job object
    Returns:
        HrflowJob: a job object in the hrflow job format
    """
    data = breezy_jobs
    job = dict()
    # Basic information
    job["name"] = data.get("name")
    job["reference"] = data.get("friendly_id")
    job["summary"] = None

    # Location
    location = data.get("location")
    country = location.get("country")
    country_name = country.get("name")
    city = location.get("city")
    address = location.get("name")
    geojson = dict(country=country_name, city=city)

    job["location"] = dict(text=address, geojson=geojson, lat=None, lng=None)

    # Sections
    description = remove_html_tags(data.get("description"))
    cleaned_description = description.replace("&nbsp;", " ")
    job["sections"] = [
        dict(
            name="breezy_hr_description",
            title="Breezy_hr_description",
            description=cleaned_description,
        )
    ]
    # tags
    job["tags"] = []

    def create_tag(field_name: str):
        tag_name = "breezy_hr_{}".format(field_name)
        tag_value = data.get(field_name)

        if isinstance(tag_value, dict):
            tag_name_value = tag_value.get("name")
            tag = dict(name=tag_name, value=tag_name_value)
            job["tags"].append(tag)
        if isinstance(tag_value, str):
            tag = dict(name=tag_name, value=tag_value)
            job["tags"].append(tag)

    create_tag("type")
    create_tag("experience")
    create_tag("education")
    create_tag("department")
    create_tag("requisition_id")
    create_tag("category")
    create_tag("candidate_type")
    is_remote = dict(name="breezy_hr_remote", value=location.get("is_remote"))
    job["tags"].append(is_remote)

    job["created_at"] = data.get("creation_date")
    job["updated_at"] = data.get("updated_date")

    return job


def format_profile(hrflow_profile: HrFlowProfile) -> BreezyProfileModel:
    """
    Format a Hrflow profile object into a breezy hr profile object
    Args:
        data (HrflowProfile): Hrflow Profile to format
    Returns:
        BreezyProfileModel: a BreezyHr formatted profile object
    """

    profile = dict()
    data = hrflow_profile
    info = data.get("info")
    profile["name"] = info.get("full_name")
    profile["address"] = info.get("location").get("text")
    profile["email_address"] = info.get("email")
    profile["phone_number"] = info.get("phone")
    profile["summary"] = info.get("summary")
    profile["work_history"] = []

    def format_experiences():

        experiences = data.get("experiences")
        for experience in experiences:
            format_experience = dict()
            if experience["company"] not in ["", None]:
                format_experience["company_name"] = experience["company"]
            else:
                format_experience["company_name"] = "Undefined"
            format_experience["title"] = experience["title"]
            format_experience["summary"] = experience["description"]
            if experience["date_start"] is not None:
                date_iso = from_str_to_datetime((experience["date_start"]))
                format_experience["start_year"] = date_iso.year
                format_experience["start_month"] = date_iso.month
            if experience["date_end"] is not None:
                date_end_iso = from_str_to_datetime((experience["date_end"]))
                format_experience["end_year"] = date_end_iso.year
                format_experience["end_month"] = date_end_iso.month

            profile["work_history"].append(format_experience)

    format_experiences()

    profile["education"] = []

    def format_educations():
        educations = data.get("educations")
        for education in educations:
            format_education = dict()
            if education["school"] == "":
                education["school"] = "Undefined"
            format_education["school_name"] = education["school"]
            format_education["field_of_study"] = education["title"]
            if education["date_start"] is not None:
                date_iso = from_str_to_datetime((education["date_start"]))
                format_education["start_year"] = date_iso.year
            if education["date_end"] is not None:
                date_end_iso = from_str_to_datetime((education["date_end"]))
                format_education["end_year"] = date_end_iso.year
            profile["education"].append(format_education)

    format_educations()

    profile["social_profiles"] = {}

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
                        profile.get("social_profiles").update({type: link})
                except:
                    continue
        attachments = info.get("attachments")
        if isinstance(attachments, list):
            for attachment in attachments:
                file_name = attachment.get("file_name")
                public_url = attachment.get("public_url")
                if isinstance(public_url, str):
                    profile.get("social_profiles").update({file_name: public_url})

    format_urls()

    # add profile skills to tags
    profile["tags"] = []
    skills = data.get("skills")
    if isinstance(skills, list):
        for skill in skills:
            if isinstance(skill, dict):
                profile["tags"].append(skill["name"])

    return profile


BreezyHR = Connector(
    name="BreezyHR",
    description=(
        "Breezyhr is an end-to-end recruiting software "
        "to help you attract & hire great employees with less effort"
    ),
    url="https://breezy.hr/",
    actions=[
        ConnectorAction(
            name="pull_jobs",
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
        ),
        ConnectorAction(
            name="push_profiles",
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
        ),
    ],
)
