import base64
import typing as t

import requests

from hrflow_connectors.v2.connectors.bullhorn.utils import date_format
from hrflow_connectors.v2.connectors.bullhorn.warehouse import BullhornWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def to_int(elm: t.Any) -> int:
    if elm is None:
        return 0
    return int(elm)


def get_location(info: dict) -> dict:
    if info is not None:
        location = info.get("location")
        if location is None:
            location = dict()
        fields = location.get("fields", {})
        if fields == []:
            fields = {}
        location_dict = {
            "address1": location.get("text"),
            "address2": None,
            "city": fields.get("city"),
            "state": fields.get("country"),
            "zip": fields.get("postcode"),
        }
        return location_dict
    return None


def get_skills(data: dict) -> str:
    skills = ""
    if data.get("skills") is not None:
        for i in range(len(data["skills"]) - 1):
            skills += data["skills"][i]["name"] + ", "
        skills += data["skills"][-1]["name"]
    return skills


def get_education(education_list: list[dict]) -> list[dict]:
    educations = []
    for hrflow_education in education_list:
        location = hrflow_education["location"]
        education = {
            "id": "0",
            "candidate": {"id": None},
            "school": hrflow_education.get("school"),
            "degree": hrflow_education.get("title"),
            "comments": hrflow_education.get("description"),
            "city": location.get("text") if location else None,
            "startDate": (
                int(
                    date_format.from_str_to_datetime(
                        hrflow_education.get("date_start")
                    ).timestamp()
                )
                if hrflow_education.get("date_start")
                else None
            ),
            "endDate": (
                int(
                    date_format.from_str_to_datetime(
                        hrflow_education.get("date_end")
                    ).timestamp()
                )
                if hrflow_education.get("date_start")
                else None
            ),
        }
        educations.append(education)
    return educations


def get_experience(experience_list: list[dict]) -> list[dict]:
    experience_json = []
    for hrflow_experience in experience_list:
        experience = {
            "id": "0",
            "candidate": {"id": None},
            "companyName": hrflow_experience.get("company"),
            "title": hrflow_experience.get("title"),
            "comments": hrflow_experience.get("description"),
            "startDate": (
                int(
                    date_format.from_str_to_datetime(
                        hrflow_experience.get("date_start")
                    ).timestamp()
                )
                if hrflow_experience.get("date_start")
                else None
            ),
            "endDate": (
                int(
                    date_format.from_str_to_datetime(
                        hrflow_experience.get("date_end")
                    ).timestamp()
                )
                if hrflow_experience.get("date_end")
                else None
            ),
        }
        experience_json.append(experience)
    return experience_json


def get_attachments(
    attachment_list: list[dict],
    file_type: str = "SAMPLE",
    content_type: str = "text/plain",
    type: str = "cover",
    format: bool = False,
) -> list[dict]:
    attachments_json = []
    for hrflow_attachment in attachment_list:
        url = hrflow_attachment["public_url"]
        response = requests.get(url)
        b64 = base64.b64encode(response.content)

        attachment = {
            "externalID": "portfolio",
            "fileContent": b64.decode(),
            "fileType": file_type,
            "name": hrflow_attachment["file_name"],
            "description": "Resume file for candidate.",
            "type": type,
        }
        if format:
            attachment["format"] = "PDF"
        else:
            attachment["content_type"] = content_type
        attachments_json.append(attachment)
    return attachments_json


def format_profile(data: dict) -> dict:
    info = data.get("info")

    date_of_birth = None
    if info is not None and info.get("date_birth"):
        date_birth_field = info.get("date_birth")
        date_birth_timestamp = date_format.from_str_to_datetime(
            date_birth_field
        ).timestamp()
        date_of_birth = int(date_birth_timestamp)

    create_profile_body = {
        "id": data.get("reference"),
        "address": get_location(info),
        "certifications": None,
        "name": info.get("full_name") if info else None,
        "firstName": info.get("first_name") if info else None,
        "lastName": info.get("last_name") if info else None,
        "email": info.get("email") if info else None,
        "mobile": info.get("phone") if info else None,
        "dateOfBirth": date_of_birth,
        "experience": to_int(data.get("experiences_duration")),  # TODO
        "skillSet": get_skills(data) if data.get("skills") else None,
    }

    enrich_profile_education = get_education(data.get("educations"))
    enrich_profile_experience = get_experience(data.get("experiences"))
    enrich_profile_attachment = get_attachments(data.get("attachments"))
    # Four querys are needed to index a Candidate to Bullhorn
    # The querys's body are grouped in a profile_body_dict
    profile_body_dict = dict(
        create_profile_body=create_profile_body,
        enrich_profile_education=enrich_profile_education,
        enrich_profile_experience=enrich_profile_experience,
        enrich_profile_attachment=enrich_profile_attachment,
    )
    return profile_body_dict


def format_job(data: dict) -> dict:
    # Info
    hrflow_name = data.get("title")
    hrflow_ref = str(data.get("id"))

    # Location
    address = data.get("address")
    hrflow_fields = {
        "city": address["city"],
        "country": address["countryCode"],
        "postal_code": address["zip"],
    }
    hrflow_location = {"text": address["address1"], "fields": hrflow_fields}

    # Sections
    section_description = {
        "name": "Bullhorn_description",
        "title": "Bullhorn_description",
        "description": data["publicDescription"],
    }
    hrlflow_sections = [section_description]

    # Tags
    degree_list = data.get("degreeList")
    if degree_list:
        degree_list = ", ".join(degree_list)

    tags = []
    tags.append({"name": "durationWeeks", "value": data.get("durationWeeks")})
    tags.append({"name": "degreeList", "value": degree_list})
    tags.append({"name": "employmentType", "value": data.get("employmentType")})
    tags.append({"name": "numOpenings", "value": data.get("numOpenings")})
    tags.append({"name": "onSite", "value": data.get("onSite")})
    tags.append({"name": "salaryUnit", "value": data.get("salaryUnit")})
    tags.append({"name": "startDate", "value": data.get("startDate")})
    tags.append({"name": "status", "value": data.get("status")})
    tags.append({"name": "type", "value": data.get("type")})
    tags.append({"name": "willRelocate", "value": data.get("willRelocate")})
    tags.append({"name": "salary", "value": data.get("salary")})
    tags.append({"name": "isWorkFromHome", "value": data.get("isWorkFromHome")})
    tags.append({"name": "hoursPerWeek", "value": data.get("hoursPerWeek")})
    tags.append({"name": "hoursOfOperation", "value": data.get("hoursOfOperation")})
    tags.append({"name": "dateAdded", "value": data.get("dateAdded")})

    # Skills
    hrflow_skills = []
    skill_list = data["skillList"]
    if skill_list:
        skill_list = skill_list.split(",")
        if skill_list:
            for skill in skill_list:
                new_skill = {"name": skill, "type": "undefined", "value": None}
                hrflow_skills.append(new_skill)

    hrflow_job = {
        "name": hrflow_name,
        "reference": hrflow_ref,
        "location": hrflow_location,
        "sections": hrlflow_sections,
        "skills": hrflow_skills,
        "tags": tags,
    }

    return hrflow_job


def format_item_to_be_archived(item):
    if not isinstance(item, dict) or item is None:
        return {"reference": None}

    reference = next(iter(item.keys()), "id")
    return {"reference": str(item[reference])}


def profile_format(data: dict) -> dict:
    # Info
    first_name = data["firstName"]
    last_name = data["lastName"]
    full_name = data["name"]
    email = data["email"]
    phone = data["mobile"]
    date_birth = data["dateOfBirth"]
    gender = data["gender"]

    # Location
    location_text = data["address"]["address1"]
    location = {"text": location_text}

    info = {
        "full_name": full_name,
        "first_name": first_name,
        "last_name": last_name,
        "email": email,
        "phone": phone,
        "date_birth": date_birth,
        "location": location,
        "gender": gender,
    }

    # Tags
    tags = []
    tags.append({"name": "dateAvailable", "value": data.get("dateAvailable")})
    tags.append({"name": "status", "value": data.get("status")})
    tags.append({"name": "employeeType", "value": data.get("employeeType")})
    tags.append(
        {"name": "activePlacements", "value": data.get("activePlacements").get("total")}
    )

    # Skills
    hrflow_skills = []
    skill_list = data["skillSet"]
    if skill_list:
        skill_list = skill_list.split(",")
        if skill_list:
            for skill in skill_list:
                new_skill = {"name": skill, "type": "hard", "value": None}
                hrflow_skills.append(new_skill)

    # Education
    hrflow_education = []
    if data.get("educations") is None:
        data["educations"] = []
    for education in data["educations"]:
        location = {"text": education["city"], "lng": None, "lat": None}
        school = education["school"]
        date_start = education["startDate"]
        date_end = education["endDate"]
        title = education["degree"]
        certifications = [education["certification"]]
        description = education["comments"]
        object_education = {
            "location": location,
            "school": school,
            "date_start": date_start,
            "date_end": date_end,
            "title": title,
            "certifications": certifications,
            "description": description,
        }
        hrflow_education.append(object_education)

    hrflow_experience = []
    if data.get("workHistories") is None:
        data["workHistories"] = []
    for experience in data["workHistories"]:
        location = {"text": "", "lng": None, "lat": None}
        company = experience["companyName"]
        date_start = experience["startDate"]
        date_end = experience["endDate"]
        title = experience["title"]
        description = experience["comments"]
        title = experience["title"]
        object_experience = {
            "title": title,
            "location": location,
            "company": company,
            "date_start": date_start,
            "date_end": date_end,
            "description": description,
        }
        hrflow_experience.append(object_experience)

    profile = {
        "reference": str(data.get("id")),
        "info": info,
        "skills": hrflow_skills,
        "experiences": hrflow_experience,
        "educations": hrflow_education,
        "created_at": None,
        "tags": tags,
        "metadatas": [],
        "resume": {"raw": data["cvFile"], "content_type": "application/pdf"},
    }

    return profile


def format_application(data: dict) -> dict:
    info = data.get("info") or {}
    attachments = (
        [data["attachments"][0]] if data.get("attachments") is not None else []
    )
    profile = {
        "firstName": info.get("first_name"),
        "lastName": info.get("last_name"),
        "name": info.get("full_name"),
        "address": get_location(info),
        "email": info.get("email"),
        "mobile": info.get("phone"),
        "source": "Hrflow's {source_name}".format(
            source_name=data.get("source", {}).get("name", "")
        ),
    }

    attachment_list = get_attachments(
        attachments, file_type="RESUME", type="RESUME", format=True
    )

    profile["attachment"] = attachment_list[0] if len(attachment_list) > 0 else {}
    return profile


DESCRIPTION = "Transform Your Business with Bullhorn Staffing and Recruitment Software"

Bullhorn = Connector(
    name="Bullhorn",
    type=ConnectorType.ATS,
    subtype="bullhorn",
    description=DESCRIPTION,
    url="https://www.bullhorn.com/",
    warehouse=BullhornWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.update,
            Entity.job,
            Direction.inbound,
            format=format_item_to_be_archived,
        ),
        Flow(Mode.create, Entity.profile, Direction.inbound, format=profile_format),
        Flow(Mode.update, Entity.profile, Direction.inbound, format=profile_format),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_item_to_be_archived,
        ),
    ),
)
