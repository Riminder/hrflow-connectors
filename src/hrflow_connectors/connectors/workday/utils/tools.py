from datetime import datetime
import requests
import typing as t

from hrflow_connectors.connectors.workday.schemas import (
    WorkdayCandidate,
    WorkdayDescriptorId,
    WorkdayEducation,
    WorkdayExperience,
    WorkdayLanguage,
    WorkdayName,
    WorkdayPhone,
    WorkdayResumeAttachments,
    WorkdaySkill,
)


def _workday_job_location_get(workday_location: t.Dict) -> t.Dict:
    text = workday_location["descriptor"]
    for key in ["region", "country"]:
        val = workday_location[key]
        if not val:
            continue
        des = workday_location[key][val]["descriptor"]
        if des:
            text += f", {des}"
    hrflow_location = dict(text=text)
    return hrflow_location


def _workday_job_tags_get(workday_job: t.Dict) -> t.List[t.Dict]:
    T = []
    if workday_job["remoteType"]:
        value = workday_job["remoteType"]["name"]
        if value:
            T.append(dict(name="remoteType", value=value))
    if workday_job["categories"]:
        for ii, category in enumerate(workday_job["categories"]):
            T.append(dict(name=f"category{ii}", value=category["descriptor"]))
    if workday_job["spotlightJob"] is not None:
        T.append(dict(name="spotlightJob", value=workday_job["spotlightJob"]))
    for key in ["timeType", "jobType"]:
        if workday_job[key]:
            des = workday_job[key]["descriptor"]
            if des:
                T.append(dict(name=key, value=des))
    return T


def _workday_job_metadatas_get(workday_job: t.Dict) -> t.List[t.Dict]:
    M = []
    if workday_job["additionalLocations"]:
        for ii, location in enumerate(workday_job["additionalLocations"]):
            value = _workday_job_location_get(location).get("text")
            M.append(dict(name=f"additionalLocation{ii}", value=value))
    for key in ["company", "jobSite"]:
        if workday_job[key]:
            des = workday_job[key]["descriptor"]
            if des:
                M.append(dict(name=key, value=des))
    return M


def _workday_ranges_date_get(workday_job: t.Dict) -> t.List[t.Dict]:
    R = []
    if workday_job["endDate"] and workday_job["startDate"]:
        R.append(
            dict(
                name="jobPostingDates",
                value_min=workday_job["startDate"],
                value_max=workday_job["endDate"],
            )
        )
    return R


def _hrflow_profile_candidate_get(hrflow_profile: t.Dict) -> t.Dict:
    info = hrflow_profile["info"]
    candidate_model = WorkdayCandidate(
        email=info["email"],
        phone=WorkdayPhone(phoneNumber=info["WorkdayPhone"]),
        name=WorkdayName(
            fullName=info["full_name"],
            firstName=info["first_name"],
            lastName=info["last_name"],
        ),
    )
    candidate = candidate_model.model_dump(exclude_node=True)
    return candidate


def _hrflow_profile_tags_get(hrflow_profile: t.Dict) -> t.Dict:
    T = []
    for tag in hrflow_profile["tags"]:
        workday_tag = WorkdayDescriptorId(id=tag["name"], descriptor=tag["value"])
        T.append(workday_tag.model_dump())
    return T


def _workday_skill_get(hrflow_skill: t.Dict) -> t.Dict:
    id_ = hrflow_skill["type"]
    val = hrflow_skill["value"]
    if val:
        id_ += f" - {val}"
    model = WorkdaySkill(name=hrflow_skill["name"], id=id_)
    skill = model.model_dump()
    return skill


def _hrflow_profile_skills_get(hrflow_profile: t.Dict) -> t.Dict:
    S = []
    for skill in hrflow_profile["skills"]:
        S.append(_workday_skill_get(skill))
    return S


def _hrflow_profile_languages_get(hrflow_profile: t.Dict) -> t.Dict:
    L = []
    for language in hrflow_profile["languages"]:
        workday_language = WorkdayLanguage(  # TODO give workday language id
            id=language["name"], native=language["value"] == "native"
        )
        L.append(workday_language.model_dump())
    return L


def _hrflow_profile_educations_get(hrflow_profile: t.Dict) -> t.Dict:
    E = []
    for education in hrflow_profile["educations"]:
        workday_education = WorkdayEducation(
            schoolName=education["school"],
            firstYearAttended=datetime.fromisoformat(education["date_start"]),
            lastYearAttended=datetime.fromisoformat(education["date_end"]),
        )
        E.append(workday_education.model_dump())
    return E


def _hrflow_profile_experiences_get(hrflow_profile: t.Dict) -> t.Dict:
    E = []
    for experience in hrflow_profile["experiences"]:
        date_start = datetime.fromisoformat(experience["date_start"][:16])
        date_end = datetime.fromisoformat(experience["date_end"][:16])
        workday_experience = WorkdayExperience(
            companyName=experience["company"],
            title=experience["title"],
            location=experience["location"]["text"],
            startYear=date_start,
            startMonth=date_start.month,
            endMonth=date_end.month,
            endYear=date_end,
        )
        E.append(workday_experience.model_dump())
    return E


def _hrflow_profile_resume_get(hrflow_profile: t.Dict) -> t.Dict:
    for attachment in hrflow_profile["attachments"]:
        if attachment["type"] != "resume":
            continue
        url = attachment["public_url"]
        if not url:
            continue
        response = requests.get(url)
        if response.status_code != requests.codes.ok:
            continue
        # TODO eventually add contentType application/pdf's workday id
        workday_resume = WorkdayResumeAttachments(
            fileLength=len(response.content),
            fileName=url.split("/")[-1],
            descriptor=response.content,
            id=url,
        )
        resume = workday_resume.model_dump()
        return resume


def _hrflow_profile_extracted_skills_get(hrflow_profile: t.Dict) -> t.List[t.Dict]:
    S = []
    for experience in hrflow_profile["experiences"]:
        skill = experience["skill"]
        if skill:
            S.append(_workday_skill_get(skill))
    for education in hrflow_profile["educations"]:
        for skill in education["skills"]:
            S.append(_workday_skill_get(skill))
    return S
