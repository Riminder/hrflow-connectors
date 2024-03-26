import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.smartrecruiters.warehouse import (
    SmartRecruitersJobWarehouse,
    SmartRecruitersProfileWarehouse,
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


def get_job_location(smartrecruiters_location: t.Union[t.Dict, None]) -> t.Dict:
    if smartrecruiters_location is None:
        return dict(lat=None, lng=None, text="")

    lat = smartrecruiters_location.get("latitude")
    lat = float(lat) if lat is not None else lat

    lng = smartrecruiters_location.get("longitude")
    lng = float(lng) if lng is not None else lng

    concatenate = []
    for field in ["country", "region", "city", "address"]:
        if smartrecruiters_location.get(field):
            concatenate.append(smartrecruiters_location.get(field))

    return dict(lat=lat, lng=lng, text=" ".join(concatenate))


def get_sections(smartrecruiters_job: t.Dict) -> t.List[t.Dict]:
    sections = []
    if (
        "jobAd" not in smartrecruiters_job
        or "sections" not in smartrecruiters_job["jobAd"]
    ):
        return sections

    smartrecruiters_sections = smartrecruiters_job["jobAd"]["sections"]
    for section_name in [
        "companyDescription",
        "jobDescription",
        "qualifications",
        "additionalInformation",
    ]:
        section = smartrecruiters_sections.get(section_name)
        if section is not None:
            sections.append(
                dict(
                    name="smartrecruiters_jobAd-sections-{}".format(section_name),
                    title=section.get("title"),
                    description=section.get("text"),
                )
            )


def get_tags(smartrecruiters_job: t.Dict) -> t.List[t.Dict]:
    job = smartrecruiters_job
    creator = job.get("creator", {})
    compensation = job.get("compensation", {})

    t = lambda name, value: dict(name=name, value=value)
    return [
        t("smartrecruiters_status", job.get("status")),
        t("smartrecruiters_postingStatus", job.get("postingStatus")),
        t("smartrecruiters_id", job.get("id")),
        t(
            "smartrecruiters_experienceLevel-id",
            job.get("experienceLevel", {}).get("id"),
        ),
        t(
            "smartrecruiters_typeOfEmployment-id",
            job.get("typeOfEmployment", {}).get("id"),
        ),
        t("smartrecruiters_compensation-min", compensation.get("min")),
        t("smartrecruiters_compensation-max", compensation.get("max")),
        t("smartrecruiters_compensation-currency", compensation.get("currency")),
        t("smartrecruiters_industry-id", job.get("industry", {}).get("id")),
        t("smartrecruiters_creator-firstName", creator.get("firstName")),
        t("smartrecruiters_creator-lastName", creator.get("lastName")),
        t("smartrecruiters_function-id", job.get("function", {}).get("id")),
        t("smartrecruiters_department-id", job.get("department", {}).get("id")),
        t("smartrecruiters_location-manual", job.get("location", {}).get("manual")),
        t("smartrecruiters_location-remote", job.get("location", {}).get("remote")),
        t("smartrecruiters_eeoCategory-id", job.get("eeoCategory", {}).get("id")),
        t("smartrecruiters_targetHiringDate", job.get("targetHiringDate")),
    ]


def format_job(smartrecruiters_job: t.Dict) -> t.Dict:
    job = dict(
        name=smartrecruiters_job.get("title", "Undefined"),
        reference=smartrecruiters_job.get("refNumber"),
        created_at=smartrecruiters_job.get("createdon"),
        updated_at=smartrecruiters_job.get("updatedon"),
        location=get_job_location(smartrecruiters_job.get("location")),
        url=None,
        summary=None,
        sections=get_sections(smartrecruiters_job),
        tags=get_tags(smartrecruiters_job),
    )
    return job


def get_profile_location(hrflow_location: t.Dict) -> t.Dict:
    fields = hrflow_location["fields"] or {}
    return dict(
        lat=hrflow_location["lat"] or 0,
        lng=hrflow_location["lng"] or 0,
        city=fields.get("city") or "Undefined",
        country=fields.get("country") or "Undefined",
        region=fields.get("state") or "Undefined",
    )


def get_profile_occupation(hrflow_occupation: t.Dict) -> t.Dict:
    return dict(
        description=hrflow_occupation["description"],
        # FIXME Maybe that current could be True for the most recent occupation
        current=False,
        startDate=(hrflow_occupation["date_start"] or "XXXX").split("T")[0],
        endDate=(hrflow_occupation["date_end"] or "XXXX").split("T")[0],
        location=hrflow_occupation["location"]["text"] or "Undefined",
    )


def get_profile_experiences(hrflow_experiences: t.List[t.Dict]) -> t.List[t.Dict]:
    return [
        dict(
            title=experience["title"] or "Undefined",
            company=experience["company"] or "Undefined",
            **get_profile_occupation(experience),
        )
        for experience in hrflow_experiences
    ]


def get_profile_educations(hrflow_educations: t.List[t.Dict]) -> t.List[t.Dict]:
    return [
        dict(
            institution=education["school"] or "Undefined",
            degree=education["title"] or "Undefined",
            major="Undefined",
            **get_profile_occupation(education),
        )
        for education in hrflow_educations
    ]


def format_profile(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_profile_info = hrflow_profile["info"]

    profile = dict(
        firstName=hrflow_profile_info["first_name"],
        lastName=hrflow_profile_info["last_name"],
        email=hrflow_profile_info["email"],
        phoneNumber=hrflow_profile_info["phone"],
        location=get_profile_location(hrflow_profile_info["location"]),
        experiences=get_profile_experiences(hrflow_profile["experiences"]),
        educations=get_profile_educations(hrflow_profile["educations"]),
        web=dict(hrflow_profile_info["urls"]),
        tags=[],
        consent=True,
        attachments=hrflow_profile.get("attachments") or [],
    )
    return profile


def format_candidate_location(
    smartrecruiters_location: t.Union[t.Dict, None]
) -> t.Dict:
    if smartrecruiters_location is None:
        return dict(lat=None, lng=None, text="")

    lat = smartrecruiters_location.get("latitude")
    lat = float(lat) if lat is not None else lat

    lng = smartrecruiters_location.get("longitude")
    lng = float(lng) if lng is not None else lng

    concatenate = []
    for field in ["country", "region", "city"]:
        if smartrecruiters_location.get(field):
            concatenate.append(smartrecruiters_location.get(field))

    return dict(lat=lat, lng=lng, text=" ".join(concatenate))


def format_candidate_urls(smartrecruiters_web_object):
    urls = []
    for key, value in smartrecruiters_web_object.items():
        if value:
            urls.append(dict(url=value, type=key))
    return urls


def format_candidate_experiences(smartrecruiters_experience):
    experiences = []
    for experience in smartrecruiters_experience:
        experiences.append(
            {
                "company": experience.get("company"),
                "description": experience.get("description"),
                "date_end": experience.get("endDate"),
                "date_start": experience.get("startDate"),
                "title": experience.get("title"),
                "location": dict(lat=None, lng=None, text=experience.get("location")),
            }
        )
    return experiences


def format_candidate_educations(smartrecruiters_education):
    educations = []
    for education in smartrecruiters_education:
        educations.append(
            {
                "school": education["institution"],
                "title": ",".join([education["major"], education["degree"]]),
                "date_start": education["startDate"],
                "date_end": education["endDate"],
                "description": education["description"],
                "location": dict(lat=None, lng=None, text=None),
            }
        )
    return educations

def format_candidate_full_name(hrflow_profile):
    first_name = hrflow_profile["info"].get("first_name", "")
    last_name = hrflow_profile["info"].get("last_name", "")
    
    return " ".join(filter(None, [first_name, last_name]))

def format_candidate(smartrecruiters_candidate):
    first_name = smartrecruiters_candidate.get("firstName")
    last_name = smartrecruiters_candidate.get("lastName")
    hrflow_profile = dict()
    hrflow_profile["reference"] = smartrecruiters_candidate.get("id")
    hrflow_profile["info"] = dict()
    hrflow_profile["info"]["first_name"] = first_name
    hrflow_profile["info"]["last_name"] = last_name
    hrflow_profile["info"]["full_name"] = " ".join(filter(None, [first_name, last_name]))
    hrflow_profile["info"]["email"] = smartrecruiters_candidate.get("email")
    hrflow_profile["info"]["phone"] = smartrecruiters_candidate.get("phoneNumber")
    hrflow_profile["info"]["location"] = format_candidate_location(
        smartrecruiters_candidate.get("location")
    )
    hrflow_profile["info"]["urls"] = format_candidate_urls(
        smartrecruiters_candidate.get("web")
    )
    hrflow_profile["created_at"] = smartrecruiters_candidate.get("createdOn")
    hrflow_profile["updated_at"] = smartrecruiters_candidate.get("updatedOn")
    hrflow_profile["experiences"] = format_candidate_experiences(
        smartrecruiters_candidate.get("experience")
    )
    hrflow_profile["educations"] = format_candidate_educations(
        smartrecruiters_candidate.get("education")
    )
    hrflow_profile["skills"] = []
    hrflow_profile["tags"] = []
    return hrflow_profile


DESCRIPTION = (
    "Move beyond applicant tracking systems (ATS) with an enterprise-grade recruiting"
    " platform designed for the modern workforce. SmartRecruiters' Talent Acquisition"
    " Suite provides everything needed to attract, select, and hire great talent."
)
SmartRecruiters = Connector(
    name="SmartRecruiters",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://www.smartrecruiters.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the ***SmartRecruiter*** API and send them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=SmartRecruitersJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from Hrflow.ai Source to SmartRecruiters via the API"
                " for the given `job_id`."
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=SmartRecruitersProfileWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all profiles via the ***SmartRecruiter*** API and send them"
                " to a ***Hrflow.ai Source***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfilesActionParameters", format=format_candidate
            ),
            origin=SmartRecruitersProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
