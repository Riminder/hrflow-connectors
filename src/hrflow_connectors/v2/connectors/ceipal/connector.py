from hrflow_connectors.v2.connectors.ceipal.warehouse import CeipalWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def job_ranges(pay_rates: dict) -> list[dict]:
    hrflow_ranges = []
    for range in pay_rates:
        min_value, max_value = range["pay_rate"].split("-")
        hrflow_ranges.append(
            {
                "name": "pay_rate_{}".format(range["pay_rate_employment_type"]),
                "min_value": min_value,
                "max_value": max_value,
                "unit": range["pay_rate_currency"],
            }
        )
    return hrflow_ranges


def job_tags(ceipal_job: dict) -> list[dict]:
    hrflow_tags = []
    tags = [
        "business_unit_id",
        "assigned_recruiter",
        "posted_by",
        "duration",
        "experience",
        "min_experience",
        "job_start_date",
        "job_end_date",
        "work_authorization",
        "number_of_positions",
        "closing_date",
        "remote_opportunities",
        "public_job_title",
        "employment_type",
        "primary_recruiter",
        "department",
        "currency",
        "job_status",
        "industry",
        "tax_terms",
        "apply_job",
        "apply_job_without_registration",
        "contact_person",
        "posted",
    ]
    hrflow_tags = [
        {"name": tag, "value": ceipal_job[tag]} for tag in tags if ceipal_job[tag]
    ]
    return hrflow_tags


def format_job(ceipal_job: dict) -> dict:
    hrflow_job = dict(
        reference=ceipal_job["id"],
        name=ceipal_job["position_title"],
        url=ceipal_job["apply_job"],
        summary=ceipal_job["requisition_description"],
        created_at=ceipal_job["created"],
        updated_at=ceipal_job["modified"],
        sections=[
            {
                "name": "description",
                "title": "description",
                "description": ceipal_job["requisition_description"],
            },
            {
                "name": "public_job_desc",
                "title": "public_job_desc",
                "description": ceipal_job["public_job_desc"],
            },
            {
                "name": "experience",
                "title": "experience",
                "description": ceipal_job["experience"],
            },
            {
                "name": "min_experience",
                "title": "min_experience",
                "description": ceipal_job["min_experience"],
            },
        ],
        location=dict(
            text=ceipal_job["address"],
            lat=None,
            lng=None,
            fields=dict(
                city=ceipal_job["city"],
                state=ceipal_job["state"],
                country=ceipal_job["country"],
                postcode=ceipal_job["postal_code"],
            ),
        ),
        skills=[
            {"name": skill, "value": None, "type": None}
            for skill in ceipal_job["skills"].split(",")
        ],
        ranges_float=job_ranges(ceipal_job["pay_rates"]),
        ranges_date=[
            {
                "name": "Period",
                "value_min": ceipal_job["job_start_date"],
                "value_max": ceipal_job["job_end_date"],
            }
        ],
        tags=job_tags(ceipal_job),
    )

    return hrflow_job


def profile_tags(ceipal_profile: dict) -> list[dict]:
    hrflow_tags = []
    tags = [
        "applicant_id",
        "middlename",
        "consultant_name",
        "email_address_1",
        "other_phone",
        "applicant_status",
        "job_title",
        "home_phone_number",
        "work_phone_number",
        "created_by",
        "source",
    ]
    hrflow_tags = [
        {"name": tag, "value": ceipal_profile[tag]}
        for tag in tags
        if ceipal_profile[tag]
    ]
    return hrflow_tags


def format_profile(ceipal_profile: dict) -> dict:
    hrflow_profile = dict(
        reference=ceipal_profile["id"],
        created_at=ceipal_profile["created_at"],
        info=dict(
            first_name=ceipal_profile["firstname"],
            last_name=ceipal_profile["lastname"],
            full_name=ceipal_profile["firstname"]
            + " "
            + ceipal_profile["middlename"]
            + " "
            + ceipal_profile["lastname"],
            email=ceipal_profile["email"],
            phone=ceipal_profile["mobile_number"],
            location=dict(
                text=ceipal_profile["address"],
                lat=None,
                lng=None,
                fields=dict(
                    city=ceipal_profile["city"],
                    state=ceipal_profile["state"],
                    country=ceipal_profile["country"],
                ),
            ),
        ),
        skills=[
            {"name": skill, "value": None, "type": None}
            for skill in ceipal_profile["skills"].split(",")
        ],
        experiences=[],
        educations=[],
        tags=profile_tags(ceipal_profile),
        resume=dict(raw=ceipal_profile.get("resume", None)),
    )

    return hrflow_profile


def format_item_for_archive(ceipal_item: dict) -> dict:
    return dict(reference=ceipal_item["id"])


DESCRIPTION = (
    "Ceipal ATS Software is the best recruiting and talent acquisition software"
    "offering intelligent, integrated, scalable staffing software solutions."
)

Ceipal = Connector(
    name="Ceipal",
    type=ConnectorType.ATS,
    subtype="ceipal",
    description=DESCRIPTION,
    url="https://www.ceipal.com",
    warehouse=CeipalWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.archive, Entity.job, Direction.inbound, format=format_item_for_archive
        ),
        Flow(Mode.create, Entity.profile, Direction.inbound, format=format_profile),
        Flow(Mode.update, Entity.profile, Direction.inbound, format=format_profile),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_item_for_archive,
        ),
    ),
)
