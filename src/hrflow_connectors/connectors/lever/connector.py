import typing as t
from datetime import datetime

from bs4 import BeautifulSoup

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.lever.warehouse import (
    LeverJobWarehouse,
    LeverProfileWarehouse,
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


def extract_job_sections(contents: list) -> str:
    lists = contents.get("lists", [])
    sections = []
    for item in lists:
        content = item["content"]
        content_text = html_to_plain_text(content)
        section = dict(
            name=item.get("text", None),
            title=item.get("text", None),
            description=content_text,
        )
        if section:
            sections.append(section)
    return sections


def html_to_plain_text(html: str) -> str:
    if not html:
        return None
    soup = BeautifulSoup(html, "html.parser")
    plain_text = soup.get_text(separator="\n")
    return plain_text


def get_job_tags(lever_job: dict) -> list:
    tags = []

    tags.append(
        {
            "name": "lever_confidential",
            "value": (
                "False"
                if lever_job.get("confidentiality", "") == "non-confidential"
                else "True"
            ),
        }
    )

    categories = lever_job.get("categories", {})
    tags.extend(
        {"name": f"lever_{tag_name}", "value": tag_value}
        for tag_name, tag_value in categories.items()
    )

    additional_tags = lever_job.get("tags", [])
    tags.extend(
        {"name": f"lever_additional_tag_{i}", "value": tag}
        for i, tag in enumerate(additional_tags)
    )

    distribution_channels = lever_job.get("distributionChannels", [])
    tags.extend(
        {"name": f"lever_distributionChannel_{index}", "value": channel}
        for index, channel in enumerate(distribution_channels)
    )

    urls = lever_job.get("urls", {})
    tags.extend(
        {"name": f"lever_url_{item}", "value": value} for item, value in urls.items()
    )

    country = lever_job.get("country")
    if country:
        tags.append({"name": "lever_country", "value": country})

    state = lever_job.get("state")
    if state:
        tags.append({"name": "lever_state", "value": state})
    workplaceType = lever_job.get("workplaceType")
    if workplaceType:
        tags.append({"name": "lever_workplaceType", "value": workplaceType})

    return tags


# get ranges float
def get_job_ranges_float(lever_job: dict) -> list:
    ranges_list = []
    ranges_float = {}
    salary_range = lever_job.get("salaryRange", {})
    if salary_range:
        ranges_float["name"] = "salary"
        ranges_float["value_min"] = salary_range.get("min", None)
        ranges_float["value_max"] = salary_range.get("max", None)
        ranges_float["unit"] = salary_range.get("currency", None)
        ranges_list.append(ranges_float)
    return ranges_list


def format_job(lever_job: dict) -> dict:
    lever_job_data = lever_job

    created_at = datetime.fromtimestamp(
        int(lever_job_data["createdAt"]) / 1000
    ).isoformat()
    updated_at = datetime.fromtimestamp(
        int(lever_job_data["updatedAt"]) / 1000
    ).isoformat()

    job = dict(
        reference=lever_job_data["id"],
        name=lever_job_data["text"],
        location=dict(
            text=(
                lever_job_data.get("categories", {}).get("location", None)
                if lever_job_data.get("categories", {}).get("location", None)
                else None
            ),
            lat=None,
            lng=None,
        ),
        url=lever_job_data["urls"]["show"],
        summary=lever_job_data["content"]["description"],
        sections=extract_job_sections(lever_job_data["content"]),
        tags=get_job_tags(lever_job_data),
        ranges_float=get_job_ranges_float(lever_job_data),
        created_at=created_at,
        updated_at=updated_at,
    )
    return job


def get_profile_experiences(lever_profile_data: t.Dict[str, any]) -> t.List:
    if not lever_profile_data:
        return []
    experiences_data = lever_profile_data.get("parsedData", {}).get("positions", [])
    experiences = []

    for experience_data in experiences_data:
        company = experience_data.get("org")
        title = experience_data.get("title")
        description = experience_data.get("summary")
        location = dict(
            text=experience_data.get("location"),
            lat=None,
            lng=None,
        )
        start_data = experience_data.get("start", {})
        end_data = experience_data.get("end", {})
        start_year = start_data.get("year")
        start_month = start_data.get("month")
        end_year = end_data.get("year")
        end_month = end_data.get("month")

        start_date = (
            datetime(start_year, start_month, 1).isoformat()
            if start_year and start_month
            else None
        )
        end_date = (
            datetime(end_year, end_month, 1).isoformat()
            if end_year and end_month
            else None
        )

        experience = dict(
            company=company,
            title=title,
            description=description,
            location=location,
            date_start=start_date,
            date_end=end_date,
            skills=[],
        )
        experiences.append(experience)

    return experiences


def get_profile_educations(lever_profile_data: t.Dict[str, any]) -> t.List:
    if not lever_profile_data:
        return []
    educations_data = lever_profile_data.get("parsedData", {}).get("schools", [])
    educations = []

    for education_data in educations_data:
        school = education_data.get("org")
        title = education_data.get("degree")
        description = education_data.get("summary")
        location = dict(
            text=education_data.get("location"),
            lat=None,
            lng=None,
        )
        start_data = education_data.get("start", {})
        end_data = education_data.get("end", {})
        start_year = start_data.get("year")
        start_month = start_data.get("month")
        end_year = end_data.get("year")
        end_month = end_data.get("month")
        start_date = (
            datetime(start_year, start_month, 1).isoformat()
            if start_year and start_month
            else None
        )
        end_date = (
            datetime(end_year, end_month, 1).isoformat()
            if end_year and end_month
            else None
        )

        education = dict(
            school=school,
            title=title,
            description=description,
            location=location,
            date_start=start_date,
            date_end=end_date,
            skills=[],
        )
        educations.append(education)

    return educations


def get_profile_attachments(lever_profile_data: t.Dict[str, any]) -> t.List[str]:
    attachments = []
    if not lever_profile_data:
        return []
    attachments_data = lever_profile_data.get("file", {})
    if attachments_data:
        attachment_dict = dict()
        attachment_dict["type"] = "original"
        attachment_dict["file_name"] = attachments_data["name"]
        attachment_dict["original_file_name"] = attachments_data["name"]
        attachment_dict["extension"] = attachments_data["ext"]
        attachment_dict["public_url"] = attachments_data["downloadUrl"]
        attachment_dict["file_size"] = attachments_data["size"]
        attachment_dict["created_at"] = datetime.fromtimestamp(
            int(attachments_data["uploadedAt"]) / 1000
        ).isoformat()
        attachments.append(attachment_dict)
    return attachments


def format_profile_urls(lever_links: dict) -> dict:
    urls = []
    if not lever_links:
        return []
    for link in lever_links:
        url = {"type": "from lever", "url": link}
        urls.append(url)
    return urls


def extract_first_last_name(full_name):
    names = full_name.split()
    if len(names) > 1:
        first_name = names[0]
        last_name = " ".join(names[1:])
    else:
        first_name = full_name
        last_name = None
    return first_name, last_name


def format_profile(opportunity_data: dict) -> dict:
    lever_opportunity = opportunity_data
    lever_profile = (
        lever_opportunity.get("profile")[0] if lever_opportunity.get("profile") else {}
    )
    created_at = datetime.fromtimestamp(
        int(lever_opportunity["createdAt"]) / 1000
    ).isoformat()
    updated_at = datetime.fromtimestamp(
        int(lever_opportunity["updatedAt"]) / 1000
    ).isoformat()
    first_name, last_name = extract_first_last_name(lever_opportunity.get("name"))
    profile = dict(
        reference=lever_opportunity["id"],
        updated_at=updated_at,
        created_at=created_at,
        info={
            "full_name": lever_opportunity.get("name"),
            "first_name": first_name,
            "last_name": last_name,
            "email": (
                lever_opportunity.get("emails")[0]
                if lever_opportunity.get("emails")
                else None
            ),
            "phone": (
                lever_opportunity.get("phones")[0].get("value")
                if lever_opportunity.get("phones")
                else None
            ),
            "location": (
                dict(text=lever_opportunity.get("location"), lat=None, lng=None)
                if lever_opportunity.get("location")
                else dict(text=None, lat=None, lng=None)
            ),
            "urls": format_profile_urls(lever_opportunity.get("links")),
        },
        text_language=None,
        text=(
            lever_opportunity.get("headline")
            if lever_opportunity.get("headline")
            else None
        ),
        experiences=get_profile_experiences(lever_profile),
        educations=get_profile_educations(lever_profile),
        attachments=get_profile_attachments(lever_profile),
        skills=[],
        tags=[],
    )
    return profile


# get profile urls
def get_profile_urls(urls: list) -> list:
    urls_list = []
    if not urls:
        return []
    for url_dict in urls:
        urls_list.append(url_dict["url"])
    return urls_list


# get profile skills
def get_profile_skills(skills: list) -> list:
    lever_tags = []
    if not skills:
        return []
    for skill_dict in skills:
        lever_tags.append(skill_dict["name"])
    return lever_tags


# transform date from iso to timestamp
def from_iso_to_timestamp(date: str) -> str:
    if not date:
        return None
    try:
        dt = datetime.strptime(date, "%Y-%m-%dT%H:%M:%S.%fZ")
    except ValueError:
        dt = datetime.strptime(date, "%Y-%m-%dT%H:%M:%S%z")

    timestamp = int(dt.timestamp() * 1000)  # Convert to milliseconds
    return timestamp


# construct headline
def consruct_headline(hrflow_profile: dict) -> str:
    experiences = hrflow_profile["experiences"]
    educations = hrflow_profile["educations"]
    list_of_entity_title = []
    if experiences:
        for experience in experiences:
            list_of_entity_title.append(experience["company"])
    if educations:
        for education in educations:
            list_of_entity_title.append(education["school"])
    if not list_of_entity_title:
        return None
    return " , ".join(list_of_entity_title)


def format_opportunity(hrflow_profile: dict) -> dict:
    lever_opportunity = dict()
    lever_opportunity["name"] = hrflow_profile["info"]["full_name"]
    lever_opportunity["headline"] = consruct_headline(hrflow_profile)
    lever_opportunity["location"] = hrflow_profile["info"]["location"]["text"]
    lever_opportunity["phones"] = [
        {"type": "mobile", "value": hrflow_profile["info"]["phone"]}
    ]
    lever_opportunity["emails"] = [hrflow_profile["info"]["email"]]
    lever_opportunity["links"] = get_profile_urls(hrflow_profile["info"]["urls"])
    lever_opportunity["tags"] = get_profile_skills(hrflow_profile["skills"])
    lever_opportunity["createdAt"] = from_iso_to_timestamp(hrflow_profile["created_at"])
    if hrflow_profile["attachments"]:
        lever_opportunity["file"] = hrflow_profile["attachments"][0]
    return lever_opportunity


DESCRIPTION = (
    "Lever is a modern recruitment platform that helps companies streamline their"
    " hiring process."
)
Lever = Connector(
    name="Lever",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://www.lever.co/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the Lever API and sends them to the Hrflow.ai"
                " Board."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=LeverJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from the Hrflow.ai Source to Lever via the API."
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_opportunity
            ),
            origin=HrFlowProfileWarehouse,
            target=LeverProfileWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description="Read a profile from Lever Source to Hrflow.ai via the API.",
            parameters=BaseActionParameters.with_defaults(
                "ReadProfilesParameters", format=format_profile
            ),
            origin=LeverProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
