import typing as t

from hrflow_connectors.connectors.greenhouse.warehouse import (
    GreenhouseJobWarehouse,
    GreenhouseProfileWarehouse,
)
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.hrflow.warehouse.job import remove_html_tags
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)

APPLICATION_TAG = "application_boardKey_jobReference"


def format_job(data: t.Dict) -> t.Dict:
    """
    format each job pulled from greenhouse job board into a HrFlow job object
    Returns:
        HrflowJob: job in the HrFlow job object format
    """
    job = dict()
    # name
    job["name"] = data.get("title")
    # summary
    job["summary"] = None
    # reference
    job["reference"] = str(data.get("id"))
    # url
    job["url"] = data.get("absolute_url")
    # location
    location = data.get("location").get("name")
    job["location"] = dict(text=location, lat=None, lng=None)
    # sections
    description_content = data.get("content")
    # convert the escaped description content into html format
    # description_html = html.unescape(description_content)
    # remove html tags to get clean text
    text = remove_html_tags(description_content)

    job["sections"] = [
        dict(
            name="greenhouse_description",
            title="greenhouse_description",
            description=text,
        )
    ]
    # metadata
    job["metadatas"] = data.get("metadata")
    # tags
    department = data.get("departments")
    if department not in [None, []]:
        department_name = department[0].get("name")
        department_id = str(department[0].get("id"))
    else:
        department_name = "Undefined"
        department_id = "Undefined"

    office = data.get("offices")
    if office not in [None, []]:
        office_name = office[0].get("name")
        office_id = str(office[0].get("id"))
    else:
        office_name = "Undefined"
        office_id = "Undefined"

    education = data.get("education")
    employment = data.get("employment")

    job["tags"] = [
        dict(name="greenhouse_department-name", value=department_name),
        dict(name="greenhouse_department-id", value=department_id),
        dict(name="greenhouse_office-location", value=office_name),
        dict(name="greenhouse_office-id", value=office_id),
        dict(name="greenhouse_education", value=education),
        dict(name="greenhouse_employment", value=employment),
    ]
    # updated_at
    job["updated_at"] = data.get("updated_at")
    return job


def format_profile(data: t.Dict) -> t.Dict:
    """
    Format a profile hrflow object to a greenhouse profile object
    Args:
        profile (HrflowProfile): profile object in the hrflow profile format
    Returns:
        GreenhouseProfileModel: profile in the greenhouse candidate  format
    """
    profile = dict()
    profile["applications"] = []
    tags = data.get("tags")
    applications = list(filter(lambda x: x["name"] == APPLICATION_TAG, tags))
    job_id_list = list(map(lambda x: x["value"].split("_")[1], applications))
    for i in range(0, len(job_id_list)):
        job_id_list[i] = int(job_id_list[i])
    if len(job_id_list) == 0:
        raise Exception(
            "No job_id found, tag named '{}' either none existent or name poorly"
            " formated.".format(APPLICATION_TAG)
        )
    for id in job_id_list:
        profile["applications"].append(dict(job_id=id))

    profile["first_name"] = data.get("info").get("first_name")
    profile["last_name"] = data.get("info").get("last_name")
    profile["external_id"] = data.get("reference")

    if data.get("attachments") not in [[], None]:
        profile["resume"] = data.get("attachments")[0]["public_url"]

    phone_number = data.get("info").get("phone")
    profile["phone_numbers"] = [dict(value=phone_number, type="mobile")]

    email = data.get("info").get("email")
    profile["email_addresses"] = [dict(value=email, type="personal")]

    address = data.get("info").get("location").get("text")
    profile["addresses"] = [dict(value=address, type="home")]

    profile["notes"] = data.get("text")

    def get_social_media_urls():
        urls = data["info"]["urls"]
        website_list = []
        for url in urls:
            if isinstance(url, dict):
                if url["url"] not in ["", None, []]:
                    website_list.append(dict(value=url["url"]))
        return website_list

    if get_social_media_urls() not in [[], None]:
        profile["social_media_addresses"] = get_social_media_urls()

    if data["experiences"] not in [[], None]:
        last_experience = data["experiences"][0]
        profile["company"] = last_experience["company"]
        profile["title"] = last_experience["title"]
        profile["employments"] = []
        for experience in data["experiences"]:
            if (
                experience["title"]
                and experience["company"]
                and experience["date_start"]
            ) not in ["", None]:
                profile["employments"].append(
                    dict(
                        company_name=experience["company"],
                        title=experience["title"],
                        start_date=experience["date_start"],
                        end_date=experience["date_end"],
                    )
                )
    return profile


Greenhouse = Connector(
    name="Greenhouse",
    type=ConnectorType.ATS,
    description="",
    url="https://www.greenhouse.io/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs of a board via the ***Greenhouse*** API and send"
                " them to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=GreenhouseJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from Hrflow.ai Source to Greenhouse  via the API"
                " for the given job_id(s) provided in tags."
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=GreenhouseProfileWarehouse,
            action_type=ActionType.outbound,
        ),
    ],
)
