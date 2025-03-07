import typing as t

from hrflow_connectors.v2.connectors.greenhouse.warehouse import GreenhouseWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow

APPLICATION_TAG = "application_boardKey_jobReference"


def format_greenhouse_job(data: t.Dict) -> t.Dict:
    """
    format each job pulled from greenhouse job board into a HrFlow job object
    Returns:
        HrflowJob: job in the HrFlow job object format
    """
    job = dict(
        reference=str(data.get("id")),
        name=data.get("title"),
        created_at=data.get("created_at"),
        updated_at=data.get("updated_at"),
        summary=data.get("notes"),
        url=data.get("absolute_url"),
        location=dict(text=data.get("location", {}).get("name"), lat=None, lng=None),
        sections=[
            dict(
                name="notes",
                title="notes",
                description=data.get("notes"),
            ),
            dict(
                name="education",
                title="education",
                description=data.get("education"),
            ),
            dict(
                name="employment",
                title="employment",
                description=data.get("employment"),
            ),
        ],
        metadatas=data.get("metadata"),
        tags=[
            dict(
                name="requisition_id",
                value=data.get("requisition_id"),
            ),
            dict(name="confidential", value=data.get("confidential")),
            dict(
                name="status",
                value=data.get("status"),
            ),
            dict(
                name="opened_at",
                value=data.get("opened_at"),
            ),
            dict(
                name="closed_at",
                value=data.get("closed_at"),
            ),
            *[
                dict(name="department", value=department["name"])
                for department in data.get("departments", [])
            ],
            *[
                dict(name="office", value=office["name"])
                for office in data.get("offices", [])
            ],
            dict(
                name="employment_type",
                value=data.get("custom_fields", {}).get("employment_type"),
            ),
            dict(
                name="maximum_budget",
                value=data.get("custom_fields", {}).get("budget", {}).get("value"),
            ),
            dict(
                name="is_template",
                value=data.get("is_template"),
            ),
            dict(
                name="copied_from_id",
                value=data.get("copied_from_id"),
            ),
        ],
        ranges_floats=(
            [
                dict(
                    name="salary_range",
                    value_min=data.get("custom_fields", {})
                    .get("salary_range", {})
                    .get("min_value", None),
                    value_max=data.get("custom_fields", {})
                    .get("salary_range", {})
                    .get("max_value", None),
                    unit=data.get("custom_fields", {})
                    .get("salary_range", {})
                    .get("unit", None),
                )
            ]
            if data.get("custom_fields", {}).get("salary_range")
            else []
        ),
    )
    return job


def format_hrflow_profile(data: t.Dict) -> t.Dict:
    """
    Format a profile hrflow object to a greenhouse profile object
    Args:
        profile (HrflowProfile): profile object in the hrflow profile format
    Returns:
        GreenhouseProfileModel: profile in the greenhouse candidate  format
    """
    profile = dict()
    profile["applications"] = []
    tags = data.get("tags", [])
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

    info = data.get("info", {})
    profile["first_name"] = info.get("first_name")
    profile["last_name"] = info.get("last_name")

    if data.get("attachments") not in [[], None]:
        profile["resume"] = data.get("attachments", [])[0]["public_url"]

    phone_number = info.get("phone")
    profile["phone_numbers"] = [dict(value=phone_number, type="mobile")]

    email = info.get("email")
    profile["email_addresses"] = [dict(value=email, type="personal")]

    address = info.get("location").get("text")
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


def format_greenhouse_profile(data):
    """
    Format a profile greenhouse object to a hrflow profile object
    Args:
        profile(GreenhouseProfileModel): profile object in the greenhouse profile format
    Returns:
        HrFlowProfile: profile in the hrflow profile format
    """
    home_address = next(
        (
            address["value"]
            for address in data["addresses"]
            if address["type"] == "home"
        ),
        None,
    )

    email_addresses = data.get("email_addresses", [])
    email = email_addresses[0]["value"] if email_addresses else None
    phone_numbers = data.get("phone_numbers", [])
    phone = phone_numbers[0]["value"] if phone_numbers else None

    profile = dict(
        reference=data.get("id"),
        info=dict(
            first_name=data.get("first_name"),
            last_name=data.get("last_name"),
            full_name=data.get("first_name") + " " + data.get("last_name"),
            email=email,
            phone=phone,
            location=dict(text=home_address, lat=None, lng=None),
        ),
        text=data.get("notes"),
        attachments=[
            dict(public_url=attachment["url"], type=attachment["type"])
            for attachment in data.get("attachments", [])
        ],
        educations=[
            dict(
                school=education["school_name"],
                title=education["degree"] + " " + education["discipline"],
                date_start=education["start_date"],
                date_end=education["end_date"],
            )
            for education in data.get("educations", [])
        ],
        experiences=[
            dict(
                title=employment["title"],
                company=employment["company_name"],
                date_start=employment["start_date"],
                date_end=employment["end_date"],
            )
            for employment in data.get("employments", [])
        ],
    )

    return profile


def format_greenhouse_item_for_archive(data: t.Dict) -> t.Dict:
    """
    Format a profile hrflow object to a greenhouse profile object
    Args:
        profile (HrflowProfile): profile object in the hrflow profile format
    Returns:
        GreenhouseProfileModel: profile in the greenhouse candidate  format
    """
    return dict(reference=str(data.get("id")))


def fromat_hrflow_item_for_archive(data: t.Dict) -> t.Dict:
    """
    Format a profile hrflow object to a greenhouse profile object
    Args:
        profile (HrflowProfile): profile object in the hrflow profile format
    Returns:
        GreenhouseProfileModel: profile in the greenhouse candidate  format
    """
    return dict(id=data.get("reference"))


DESCRIPTION = (
    "Greenhouse is an applicant tracking system and recruitment software that helps"
    " companies manage and streamline their hiring process. It includes features such"
    " as job posting, candidate tracking, scheduling interviews, and offer management. "
    " Greenhouse also provides analytics and reporting to help companies track the"
    " effectiveness of their recruitment process. It also offers an API to help"
    " companies integrating it with other internal systems."
)


Greenhouse = Connector(
    name="Greenhouse",
    type=ConnectorType.ATS,
    subtype="greenhouse",
    description=DESCRIPTION,
    url="https://www.greenhouse.io/",
    warehouse=GreenhouseWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_greenhouse_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_greenhouse_job),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=format_greenhouse_item_for_archive,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_greenhouse_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_greenhouse_profile,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_greenhouse_item_for_archive,
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
            format=format_hrflow_profile,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.outbound,
            format=fromat_hrflow_item_for_archive,
        ),
    ),
)
