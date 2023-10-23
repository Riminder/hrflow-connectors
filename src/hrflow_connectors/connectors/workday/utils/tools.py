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
    """Extracts location text from Workday Location."""

    text = workday_location["descriptor"]

    for key in ["region", "country"]:
        val = workday_location[key]
        if not val:
            continue
        des = workday_location[key][val]["descriptor"]
        if des:
            text += f", {des}"

    return dict(text=text)


def _workday_job_tags_get(workday_job: t.Dict) -> t.List[t.Dict]:
    """Extracts tags from a Workday Job."""

    tags = []

    if workday_job["remoteType"]:
        tags.append(
            dict(name="workday_remoteType", value=workday_job["remoteType"]["name"])
        )

    if workday_job["categories"]:
        for ii, category in enumerate(workday_job["categories"]):
            tags.append(
                dict(name=f"workday_category_{ii}", value=category["descriptor"])
            )

    if workday_job["spotlightJob"]:
        tags.append(
            dict(name="workday_spotlightJob", value=workday_job["spotlightJob"])
        )

    for key in ["timeType", "jobType"]:
        if workday_job[key]:
            tags.append(dict(name=key, value=workday_job[key]["descriptor"]))

    return tags


def _workday_job_metadatas_get(workday_job: t.Dict) -> t.List[t.Dict]:
    """Extracts metadatas from Workday Job."""

    metadatas = []

    if workday_job["additionalLocations"]:
        for ii, location in enumerate(workday_job["additionalLocations"]):
            metadatas.append(
                dict(
                    name=f"additionalLocation_{ii}",
                    value=_workday_job_location_get(location)["text"],
                )
            )

    for key in ["company", "jobSite"]:
        if workday_job[key]:
            metadatas.append(dict(name=key, value=workday_job[key]["descriptor"]))

    return metadatas


def _workday_ranges_date_get(workday_job: t.Dict) -> t.Optional[t.List[t.Dict]]:
    """Extracts date ranges from Workday Job."""

    start = workday_job.get("startDate")
    end = workday_job.get("endDate")

    if start and end:
        return [dict(name="jobPostingDates", value_min=start, value_max=end)]
    else:
        return None


def _hrflow_profile_candidate_get(hrflow_profile: t.Dict) -> t.Dict:
    """Extracts Workday Candidate from HrFlow Profile."""

    info = hrflow_profile["info"]
    candidate = WorkdayCandidate(
        email=info["email"],
        phone=WorkdayPhone(phoneNumber=info["WorkdayPhone"]),
        name=WorkdayName(
            fullName=info["full_name"],
            firstName=info["first_name"],
            lastName=info["last_name"],
        ),
    )

    return candidate.dict(exclude_none=True)


def _hrflow_profile_tags_get(hrflow_profile: t.Dict) -> t.Optional[t.List[t.Dict]]:
    """Extracts tags from HrFlow Profile."""

    if hrflow_profile.get("tags"):
        return [
            WorkdayDescriptorId(id=tag["name"], descriptor=tag["value"]).dict()
            for tag in hrflow_profile["tags"]
        ]
    else:
        return None


def _hrflow_skill_get(hrflow_skill: t.Dict) -> t.Dict:
    """Extracts skill from HrFlow Skill."""

    return WorkdaySkill(
        name=hrflow_skill["name"],
        id=hrflow_skill["type"]
        + ("" if not hrflow_skill["value"] else f" - {hrflow_skill['value']}"),
    ).dict()


def _hrflow_profile_skills_get(hrflow_profile: t.Dict) -> t.Optional[t.List[t.Dict]]:
    """Extracts skills from a HrFlow Profile."""

    if hrflow_profile["skills"]:
        return [_hrflow_skill_get(skill) for skill in hrflow_profile["skills"]]
    else:
        return None


def _hrflow_profile_languages_get(hrflow_profile: t.Dict) -> t.Optional[t.List[t.Dict]]:
    """Extract languages from a HrFlow Profile."""

    if hrflow_profile["languages"]:  # TODO give workday language id
        return [
            WorkdayLanguage(
                id=language["name"], native=language["value"] == "native"
            ).dict()
            for language in hrflow_profile["languages"]
        ]
    else:
        return None


def _hrflow_profile_educations_get(hrflow_profile: t.Dict) -> t.List[t.Dict]:
    """Extracts educations from a HrFlow Profile."""

    if hrflow_profile["educations"]:
        return [
            WorkdayEducation(
                schoolName=education["school"],
                firstYearAttended=datetime.fromisoformat(education["date_start"]),
                lastYearAttended=datetime.fromisoformat(education["date_end"]),
            ).dict()
            for education in hrflow_profile["educations"]
        ]
    else:
        return []


def _hrflow_profile_experiences_get(hrflow_profile: t.Dict) -> t.List[t.Dict]:
    """Extracts experiences from a HrFlow Profile."""

    experiences = []

    for experience in hrflow_profile["experiences"]:
        date_start = datetime.fromisoformat(experience["date_start"][:16])
        date_end = datetime.fromisoformat(experience["date_end"][:16])
        experiences.append(
            WorkdayExperience(
                companyName=experience["company"],
                title=experience["title"],
                location=experience["location"]["text"],
                startYear=date_start,
                startMonth=date_start.month,
                endMonth=date_end.month,
                endYear=date_end,
            ).dict()
        )

    return experiences


def _hrflow_profile_resume_get(hrflow_profile: t.Dict) -> t.Optional[t.Dict]:
    """Extracts the resume from a HrFlow Profile."""

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
        resume = WorkdayResumeAttachments(
            fileLength=len(response.content),
            fileName=url.split("/")[-1],
            descriptor=response.content,
            id=url,
        )

        return resume.dict()

    return None


def _hrflow_profile_extracted_skills_get(hrflow_profile: t.Dict) -> t.List[t.Dict]:
    """Extracts skills from the experiences and education of a HrFlow Profile."""

    skills = []

    for key in ["experiences", "educations"]:
        if hrflow_profile[key] and isinstance(hrflow_profile[key]["skills"], list):
            skills.extend(hrflow_profile[key]["skills"])

    return [_hrflow_skill_get(skill) for skill in skills]
