import typing as t

from hrflow_connectors.v2.connectors.zohorecruit.warehouse import ZohoRecruitWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_location(record: dict) -> dict:
    parts = []

    street = record.get("Street", None)
    city = record.get("City", None)
    state = record.get("State", None)
    country = record.get("Country", None)
    zip_code = record.get("Zip_Code", None)
    parts = [street, city, state, country, zip_code]

    location_text = " ".join([part for part in parts if part])

    location = dict(
        text=location_text,
        lat=None,
        lng=None,
        fields=dict(
            postcode=zip_code,
            city=city,
            state=state,
            country=country,
            text=location_text,
        ),
    )
    return location


def get_job_opening_tags(job_opening: dict) -> list:
    tags_list = [
        "AccountManager",
        "AssignedRecruiter",
        "ClientName",
        "ContactName",
        "Date_Opened",
        "Industry",
        "Job_Opening_Status",
        "JobType",
        "Number_of_Positions",
        "No_of_Candidates_Associated",
        "No_of_Candidates_Hired",
        "State",
        "WorkExperience",
        "Country",
        "ModifiedBy",
        "Salary",
        "Remote_Job",
    ]
    tags = []
    for tag in tags_list:
        if job_opening.get(tag):
            tags.append(dict(name=tag, value=job_opening[tag]))
    return tags


def format_zoho_job_opening_to_hrflow(job_opening: dict) -> dict:
    hrflow_job = dict(
        reference=job_opening["id"],
        created_at=job_opening["Created_Time"],
        updated_at=job_opening["Modified_Time"],
        name=job_opening["Job_Opening_Name"],
        location=get_location(job_opening),
        summary=job_opening["Job_Description"],
        skills=get_skills(job_opening["Required_Skills"]),
        # TODO: get Additional_Info & Job_Description and add them only if they exist
        sections=[
            dict(
                name="Job_Description",
                title="Job Description",
                description=job_opening["Job_Description"],
            ),
            (
                dict(
                    name="Additional_Info",
                    title="Additional Info",
                    description=job_opening.get("Additional_Info", None),
                )
            ),
        ],
        tags=get_job_opening_tags(job_opening),
    )
    return hrflow_job


def get_experiences(experiences: list) -> list:
    experiences_list = []
    for experience in experiences:
        experience_dict = dict(
            title=experience["Occupation_Title"],
            company=experience["Company"],
            location={"text": "", "lng": None, "lat": None},
            date_start=experience.get("Work_Duration", {}).get("from", None),
            date_end=experience.get("Work_Duration", {}).get("to", None),
            description=experience["Summary"],
        )
        experiences_list.append(experience_dict)
    return experiences_list


def get_educations(educations: list) -> list:
    educations_list = []
    for education in educations:
        education_dict = dict(
            title=education["Degree"],
            school=education["Institute_School"],
            location={"text": "", "lng": None, "lat": None},
            date_start=education.get("Education_Duration", {}).get("from", None),
            date_end=education.get("Education_Duration", {}).get("to", None),
            description=education["Major_Department"],
        )
        educations_list.append(education_dict)
    return educations_list


def get_skills(skill_set: str) -> list:
    skills = []
    if not skill_set:
        return skills
    extracted_skills = skill_set.split(", ")
    for skill in extracted_skills:
        skill_dict = dict(
            name=skill,
            value=None,
            type="hard",
        )
        skills.append(skill_dict)
    return skills


def get_candidate_tags(candidate: dict) -> list:
    tags_list = [
        "Current_Job_Title",
        "Current_Salary",
        "Expected_Salary",
        "Current_Employer",
        "Highest_Qualification_Held",
        "Source",
        "Origin",
        "Candidate_Status",
        "Is_Unqualified",
        "Additional_Info",
        "applied_with_linkedin",
        "No_of_Applications",
    ]
    tags = []
    for tag in tags_list:
        if candidate.get(tag):
            tags.append(dict(name=tag, value=candidate[tag]))
    return tags


def format_zoho_candidate_to_hrflow(candidate: dict) -> dict:
    hrflow_profile = dict(
        reference=candidate["id"],
        info=dict(
            first_name=candidate["First_Name"],
            last_name=candidate["Last_Name"],
            full_name=candidate["Full_Name"],
            email=candidate["Email"],
            phone=candidate["Phone"],
            location=get_location(candidate),
            urls=[
                dict(type="linkedin", url=candidate["LinkedIn__s"]),
                dict(type="facebook", url=candidate["Facebook__s"]),
                dict(type="twitter", url=candidate["Twitter"]),
                dict(type="from_resume", url=candidate["Website"]),
            ],
        ),
        created_at=candidate["Created_Time"],
        updated_at=candidate["Updated_On"],
        experiences_duration=candidate["Experience_in_Years"],
        experiences=get_experiences(candidate["Experience_Details"]),
        educations=get_educations(candidate["Educational_Details"]),
        skills=get_skills(candidate["Skill_Set"]),
        tags=get_candidate_tags(candidate),
    )
    return hrflow_profile


def get_zoho_skill_set(skills: list) -> str:
    skill_set = ""
    for skill in skills:
        skill_set += skill["name"] + ", "
    return skill_set[:-2]


def get_zoho_experiences(experiences: list) -> list:
    experiences_list = []
    for experience in experiences:
        work_duration = None
        if experience["date_start"]:
            work_duration = {
                "from": experience["date_start"],
            }
            if experience["date_end"]:
                work_duration["to"] = experience["date_end"]
        experience_dict = dict(
            Occupation_Title=experience["title"],
            I_currently_work_here=experience["date_end"] is None,
            Company=experience["company"],
            Work_Duration=work_duration,
            Summary=experience["description"],
        )
        experiences_list.append(experience_dict)
    return experiences_list


def get_zoho_educations(educations: list) -> list:
    educations_list = []
    for education in educations:
        duration = None
        if education["date_start"]:
            duration = {
                "from": education["date_start"],
            }
            if education["date_end"]:
                duration["to"] = education["date_end"]
        education_dict = dict(
            Institute_School=education["school"],
            Currently_pursuing=education["date_end"] is None,
            Degree=education["title"],
            Major_Department=education["description"],
            Duration=duration,
        )
        educations_list.append(education_dict)
    return educations_list


def get_url(urls: list, url_type: str) -> t.Optional[str]:
    for url in urls:
        if url["type"] == url_type:
            return url["url"]
    return None


def format_hrflow_profile_to_zoho(profile: dict) -> dict:
    zoho_candidate = dict(
        First_Name=profile["info"]["first_name"],
        Last_Name=profile["info"]["last_name"],
        Full_Name=profile["info"]["full_name"],
        Email=profile["info"]["email"],
        Phone=profile["info"]["phone"],
        skillSet=get_zoho_skill_set(profile["skills"]),
        Experience_in_Years=(
            int(profile["experiences_duration"])
            if profile["experiences_duration"]
            else None
        ),
        Experience_Details=get_zoho_experiences(profile["experiences"]),
        Educational_Details=get_zoho_educations(profile["educations"]),
        Created_Time=profile["created_at"][:19] if profile["created_at"] else None,
        Updated_On=profile["updated_at"][:19] if profile["updated_at"] else None,
        LinkedIn__s=get_url(profile["info"]["urls"], "linkedin"),
        Facebook__s=get_url(profile["info"]["urls"], "facebook"),
        Twitter=get_url(profile["info"]["urls"], "twitter"),
        Street=profile["info"]["location"]["text"],
        City=profile["info"]["location"]["fields"].get("city", None),
        State=profile["info"]["location"]["fields"].get("state", None),
        Zip_Code=profile["info"]["location"]["fields"].get("postcode", None),
        Country=profile["info"]["location"]["fields"].get("country", None),
    )

    return zoho_candidate


def format_hrflow_profile_for_update_to_zoho(profile: dict) -> dict:
    zoho_candidate = format_hrflow_profile_to_zoho(profile)
    zoho_candidate["id"] = int(profile["reference"])
    return zoho_candidate


def format_archive_in_hrflow(record: dict) -> dict:
    return dict(reference=record["id"])


def format_archive_in_zoho(record: dict) -> dict:
    return dict(id=record["reference"])


DESCRIPTION = (
    "Zoho Recruit offers a powerful ATS and CRM in a single recruitment platform. With"
    " scalability, customization, and remote hiring tools, Recruit has everything your"
    " staffing agency or internal HR team needs to match the right candidate to the"
    " right role."
)

ZohoRecruit = Connector(
    name="Zoho Recruit",
    type=ConnectorType.ATS,
    subtype="zohorecruit",
    description=DESCRIPTION,
    url="https://zoho.com/recruit/",
    warehouse=ZohoRecruitWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_zoho_candidate_to_hrflow,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_to_zoho,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_zoho_candidate_to_hrflow,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_for_update_to_zoho,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_archive_in_hrflow,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.outbound,
            format=format_archive_in_zoho,
        ),
        Flow(
            Mode.create,
            Entity.job,
            Direction.inbound,
            format=format_zoho_job_opening_to_hrflow,
        ),
        Flow(
            Mode.update,
            Entity.job,
            Direction.inbound,
            format=format_zoho_job_opening_to_hrflow,
        ),
        Flow(
            Mode.archive, Entity.job, Direction.inbound, format=format_archive_in_hrflow
        ),
    ),
)
