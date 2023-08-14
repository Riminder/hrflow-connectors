import datetime
import re
import typing as t
from typing import Any, Dict

from hrflow_connectors.connectors.hrflow.schemas import HrFlowProfile
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.sapsuccessfactors.warehouse import (
    SAPJobWarehouse,
    SAPProfileWarehouse,
)
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)

from .schemas import SAPSuccessFactorsJob
from .utils.datetime_converter import from_str_to_datetime

DEFAULT_COUNTRY = "France"
DEFAULT_JOB_NAME = "Undefined"
DEFAULT_ADDRESS = "Undefined"


def remove_html_tags(text: str) -> str:
    """
    Remove all HTML tags in a string
    Args:
        text (str): text to clean
    Returns:
        str: cleaned text (without HTML tags)
    """
    return re.sub("<[^<]+?>", "", text)


def format_date(date: datetime.datetime) -> str:
    """
    Format `datetime.datetime` to SAP Date
    Args:
        date (datetime.datetime): Datetime to format
    Returns:
        str: formatted date
    """
    timestamp = int(date.timestamp())
    sap_date = f"/Date({timestamp})/"
    return sap_date


def format_start_date(date_str: str) -> str:
    """
    Format ISO start date to SAP Date
    Args:
        date_str (str): iso date to format
    Returns:
        str: formatted SAP date
    """
    converted_date = from_str_to_datetime(date_str)
    return format_date(converted_date)


def format_end_date(date_str: str) -> str:
    """
    Format ISO end date to SAP Date
    Args:
        date_str (str): iso date to format
    Returns:
        str: formatted SAP date
    """
    converted_date = from_str_to_datetime(date_str)
    # add 10 seconds to avoid that end_date is less or equal than start_date
    converted_date += datetime.timedelta(seconds=10)
    return format_date(converted_date)


def format_job(data: SAPSuccessFactorsJob) -> t.Dict:
    """
    Format retrieved jobs into a HrFlow job object
    Args:
        data (SAPSuccessFactorsJob): job to format
    Returns:
        HrflowJob: job in the HrFlow job object format
    """

    job = dict()

    job_requisition = data.get("requisition")
    if job_requisition is None:
        job_requisition = dict()
    data = data.get("job")

    # name
    if data.get("jobTitle") is not None:
        job["name"] = data.get("jobTitle")
    else:
        job["name"] = DEFAULT_JOB_NAME

    # reference
    job["reference"] = data.get("jobReqId")

    # location
    geojson = dict(
        city=job_requisition.get("city"),
        country=job_requisition.get("country"),
        facility=job_requisition.get("facility"),
        province=job_requisition.get("stateProvince"),
    )
    job["location"] = dict(
        text=job_requisition.get("location"),
        city=job_requisition.get("city"),
        geojson=geojson,
        lat=None,
        lng=None,
    )

    # sections
    job["sections"] = []
    if data.get("jobDescription") is not None:
        description_text = data.get("jobDescription")
        description_text = remove_html_tags(description_text)
        description_text = description_text.replace("#13;", " ")
        description_text = description_text.replace("&", "")
        description_text = description_text.replace("&nbsp;", "")
        description_text = description_text.replace("quo;s", "")

        description_section = dict(
            name="sap_description",
            title="sap_description",
            description=description_text,
        )
        job["sections"].append(description_section)

    # tags
    t = lambda name, value: dict(name=name, value=value)
    job["tags"] = [
        t("sapsuccessfactors_annual_SA", job_requisition.get("annual_SA")),
        t("sapsuccessfactors_department", job_requisition.get("department")),
        t("sapsuccessfactors_function", job_requisition.get("function")),
        t("sapsuccessfactors_division", job_requisition.get("division")),
        t("sapsuccessfactors_industry", job_requisition.get("industry")),
        t("sapsuccessfactors_monthly_salary", job_requisition.get("monthly_salary")),
        t("sapsuccessfactors_otherBonus", job_requisition.get("otherBonus")),
        t("sapsuccessfactors_salaryBase", job_requisition.get("salaryBase")),
        t("sapsuccessfactors_salaryMax", job_requisition.get("salaryMax")),
        t("sapsuccessfactors_salaryMin", job_requisition.get("salaryMin")),
        t("sapsuccessfactors_jobStartDate", job_requisition.get("jobStartDate")),
    ]

    job["metadatas"] = [
        t("sapsuccessfactors_recruiterTeam", job_requisition.get("recruiterTeam")),
        t("sapsuccessfactors_sourcerTeam", job_requisition.get("sourcerTeam")),
        t(
            "sapsuccessfactors_hiringManagerTeam",
            job_requisition.get("hiringManagerTeam"),
        ),
    ]

    return job


def format_education(education: Dict[str, Any]) -> Dict[str, Any]:
    """
    Format Hrflow education to SAP Education
    Args:
        education (Dict[str, Any]): Hrflow education
    Returns:
        Dict[str, Any]: SAP education
    """
    sap_education = dict()
    date_start = education.get("date_start")
    date_end = education.get("date_end")

    if date_start is not None:
        sap_education["startDate"] = format_start_date(date_start)

    if date_end is not None:
        sap_education["endDate"] = format_end_date(date_end)

    sap_education["school"] = education.get("school")
    school_location = education.get("location", {})
    school_address = school_location.get("text")
    if school_address is None:
        school_address = DEFAULT_ADDRESS
    sap_education["schoolAddress"] = school_address
    return sap_education


def format_experience(experience: Dict[str, Any]) -> Dict[str, Any]:
    """
    Format Hrflow experience to SAP experience
    Args:
        experience (Dict[str, Any]): Hrflow experience
    Returns:
        Dict[str, Any]:SAP experience
    """
    sap_experience = dict()
    sap_experience["employer"] = experience.get("company")

    experience_location = experience.get("location", {})
    experience_address = experience_location.get("text")
    if experience_address is None:
        experience_address = DEFAULT_ADDRESS
    sap_experience["employerAddress"] = experience_address

    date_start = experience.get("date_start")
    date_end = experience.get("date_end")
    if date_start is not None:
        sap_experience["startDate"] = format_start_date(date_start)

    if date_end is not None:
        sap_experience["endDate"] = format_end_date(date_end)

    return sap_experience


def format_profile(profile: HrFlowProfile) -> t.Dict:
    """
    Formats an hrflow profile into a sap successfactors candidate
    Args:
        profile (HrflowProfile): profile to format
    Returns:
        SapCandidateModel: a SAP successfactors profile
    """
    sap_profile = dict()

    # Basic information
    info = profile.get("info")
    sap_profile["country"] = DEFAULT_COUNTRY
    sap_profile["address"] = info.get("location").get("text")
    sap_profile["cellPhone"] = info.get("phone")
    fields = info.get("location").get("fields")
    if fields not in [None, []]:
        sap_profile["country"] = fields.get("country")[:-1]
        sap_profile["city"] = fields.get("city")
        sap_profile["zip"] = fields.get("postcode")

    sap_profile["primaryEmail"] = info.get("email")
    sap_profile["firstName"] = info.get("first_name")
    sap_profile["lastName"] = info.get("last_name")
    sap_profile["currentTitle"] = info.get("summary")

    # educations
    if profile.get("educations") is not None:
        sap_profile["education"] = dict(results=[])
        for education in profile.get("educations", []):
            sap_education = format_education(education)
            sap_profile["education"]["results"].append(sap_education)

    # experiences
    if profile.get("experiences") is not None:
        sap_profile["outsideWorkExperience"] = dict(results=[])
        for experience in profile.get("experiences", []):
            sap_experience = format_experience(experience)
            sap_profile["outsideWorkExperience"]["results"].append(sap_experience)
    return sap_profile


DESCRIPTION = (
    "By understanding what employees need, how they work, and what motivates them, you"
    " can put people at the heart of your HR strategy."
)

SAPSuccessFactors = Connector(
    name="SAPSuccessFactors",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://www.sap.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the ***SAPSuccessFactors*** API and sends them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=SAPJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile taken from a Hrflow.ai Source to SAPSuccessFactors"
                " via the SAP API"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=SAPProfileWarehouse,
            action_type=ActionType.outbound,
        ),
    ],
)
