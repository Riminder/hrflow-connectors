import typing as t
from datetime import datetime

import requests

from hrflow_connectors.connectors.hrflow.schemas import (
    HrFlowJob,
    HrFlowProfile,
    Location,
    ProfileInfo,
    RangesDate,
)
from hrflow_connectors.connectors.oracleorc.schemas import (
    OracleORCAttachment,
    OracleORCCandidate,
    OracleORCCandidateAddress,
    OracleORCCandidatePhone,
    OracleORCEducation,
    OracleORCExperience,
    OracleORCSkill,
)


def _oracleorc_profile_assign_info(
    oracleorc_profile: OracleORCCandidate, hrflow_profile: t.Dict
) -> None:
    """
    Procedure to assign a `HrFlow.ProfileInfo` to an `OracleORC.Candidate`.

    Args:
      oracleorc_profile (OracleORCCandidate): pydantic object representing the Oracle\
 ORC Candidate
      hrflow_profile (dict): dictionary representing the hrflow profile to take profile\
 info from

    Returns:
      None
    """

    info = hrflow_profile.get("info")

    if not info:
        return

    oracleorc_profile.Email = info.get("email")
    oracleorc_profile.FirstName = info.get("first_name")
    oracleorc_profile.LastName = info.get("last_name")
    oracleorc_profile.CreatedBy = "HrFlow.ai"

    location = info.get("location")

    if location:
        fields = location.get("fields")
        address = OracleORCCandidateAddress(AddressLine1=location.get("text"))
        if isinstance(fields, dict):
            address.Building = fields.get("house")
            address.Country = fields.get("country")
            address.City = fields.get("city")
            address.FloorNumber = fields.get("level")
            address.LongPostalCode = address.PostalCode
            address.PostalCode = fields.get("postcode")
            address.Region1 = fields.get("country_region")
            address.Region2 = fields.get("state")
        oracleorc_profile.candidateAddress = [address]

    if info.get("phone"):
        oracleorc_profile.candidatePhones = [
            OracleORCCandidatePhone(PhoneNumber=info["phone"])
        ]


def _oracleorc_profile_assign_attachments(
    oracleorc_profile: OracleORCCandidate, hrflow_profile: t.Dict
) -> None:
    """
    Procedure to assign attachments of a `HrFlow.Profile` to an `OracleORC.Candidate`

    Args:
      oracleorc_profile (OracleORCCandidate): pydantic object representing the Oracle\
 ORC Candidate
      hrflow_profile (dict): dictionary representing the hrflow profile to take the\
 attachments from

    Returns:
      None
    """

    attachments = hrflow_profile.get("attachments")

    if not isinstance(attachments, list):
        return

    oracleorc_profile.attachments = list()

    for attachment in attachments:
        try:
            attachment_type = attachment["type"].lower()
            if not (attachment_type in ["resume", "cover"]):
                continue
            public_url = attachment["public_url"]
            file_name = public_url.split("/")[-1]
            file_extu = file_name.split(".")[-1].lower()
        except Exception:
            continue
        response = requests.get(public_url)
        if response.status_code != requests.codes.ok:
            continue
        try:
            A = OracleORCAttachment(
                AttachedDocumentId=len(oracleorc_profile.attachments),
                FileName=file_name,
                UploadedFileName=file_name,
                FileUrl=public_url,
                FileContents=response.content,
                UploadedFileContentType=(
                    "application/pdf" if file_extu == "pdf" else None
                ),
                UploadedFileLength=len(response.content),
                CategoryName=attachment_type,
            )

            oracleorc_profile.attachments.append(A)
        except Exception:
            pass


def _summary_get(object: t.Dict = None) -> None:
    """
    Get a summary of certifications, courses, interests, languages, skills and tasks\
 objects from given `object`.

    Args:
      object (dict): dictionnary representing an object containing objects of list of\
 certification (resp. courses, interests, languages, skills and tasks) objects

    Returns:
      str representing the summary

    >>> _summary_get(dict(skills=dict(name='Deep Learning', type='Hard', value='99/100'\
)))
    "Here is a list of skills, names, and optionally their types and values: 'Deep\
 Learning' of type 'Hard' and value '99/100'."
    >>> _summary_get(dict(languages=[dict(name='Russian', value='Native'), dict(name=\
'English')])
    "Here is a list of languages, names, and optionally their values: 'Russian' and its\
 value 'Native', 'English'."
    """

    summary = ""

    if not isinstance(object, dict):
        return

    skills = object.get("skills")
    if skills is not None:
        if not isinstance(skills, list):
            skills = [skills]
        if len(skills) > 0:
            summary += (
                " Here is a list of skills, names, and optionally their types and"
                " values: "
                + ", ".join(
                    f"'{s.get('name')}'"
                    + (f" of type '{s.get('type')}'" if s.get("type") else "")
                    + (f" and value '{s.get('value')}'" if s.get("value") else "")
                    for s in skills
                )
                + "."
            )

    for key in ["certifications", "courses", "tasks", "languages", "interests"]:
        data = object.get(key)
        if data is None:
            continue
        if not isinstance(data, list):
            data = [data]
        if len(data) == 0:
            continue
        summary += (
            f" Here is a list of {key}, names, and optionally their values: "
            + ", ".join(
                f"'{d.get('name')}'"
                + (f" and its value '{d.get('value')}'" if d.get("value") else "")
                for d in data
            )
            + "."
        )

    return summary.strip()


def _oracleorc_profile_assign_educations(
    oracleorc_profile: OracleORCCandidate, hrflow_profile: t.Dict
) -> None:
    """
    Procedure to assign list of `HrFlow.Education` objects to an `OracleORC.Candidate`

    Args:
      oracleorc_profile (OracleORCCandidate): pydantic object representing the Oracle\
 ORC Candidate
      hrflow_profile (dict): dictionary representing the hrflow profile to take the\
 education objects from

    Returns:
      None
    """

    educations = hrflow_profile.get("educations")

    if not isinstance(educations, list):
        return

    oracleorc_profile.education = list()

    for education in educations:
        try:
            E = OracleORCEducation(
                Description=education.get("description"),
                ActivitySummary=_summary_get(education),
                StartDate=education.get("date_start"),
                EndDate=education.get("date_end"),
                Title=education.get("title"),
            )

            if E.StartDate and E.EndDate:
                st = datetime.fromisoformat(E.StartDate[:16]).timestamp()
                et = datetime.fromisoformat(E.EndDate[:16]).timestamp()
                E.Duration = str(round((et - st) / (60 * 60 * 24 * 365), 2))
                E.DurationUnits = "years"

            oracleorc_profile.education.append(E)
        except Exception:
            pass


def _oracleorc_profile_assign_experiences(
    oracleorc_profile: OracleORCCandidate, hrflow_profile: t.Dict
) -> None:
    """
    Procedure to assign list of `HrFlow.Experience` objects to an `OracleORC.Candidate`

    Args:
      oracleorc_profile (OracleORCCandidate): pydantic object representing the Oracle\
 ORC Candidate
      hrflow_profile (dict): dictionary representing the hrflow profile to take the\
 experience objects from

    Returns:
      None
    """

    experiences = hrflow_profile.get("experiences")

    if not isinstance(experiences, list):
        return

    oracleorc_profile.experience = list()

    for experience in experiences:
        try:
            E = OracleORCExperience(
                JobTitle=experience.get("title"),
                StartDate=experience.get("date_start"),
                EndDate=experience.get("date_end"),
                AdditionalInformation=_summary_get(experience),
            )

            if isinstance(experience.get("tasks"), list):
                ii = E.AdditionalInformation.find("Here is a list of tasks")
                if ii >= 0:
                    resps = E.AdditionalInformation[ii:]
                    jj = resps[4:].find("Here")
                    if jj >= 0:  # crop out
                        resps = resps[: jj + 4]
                    E.Responsibilities = resps.strip()

            oracleorc_profile.experience.append(E)
        except Exception:
            pass


def _oracleorc_profile_assign_skills(
    oracleorc_profile: OracleORCCandidate, hrflow_profile: t.Dict
) -> None:
    """
    Procedure to assign list of `HrFlow.Skill` objects to an `OracleORC.Candidate`

    Args:
      oracleorc_profile (OracleORCCandidate): pydantic object representing the Oracle\
 ORC Candidate
      hrflow_profile (dict): dictionary representing the hrflow profile to take the\
 skill objects from

    Returns:
      None
    """

    skills = hrflow_profile.get("skills")

    if not isinstance(skills, list):
        return

    oracleorc_profile.skills = list()

    for skill in skills:
        oracleorc_profile.skills.append(
            OracleORCSkill(
                Skill=skill.get("name"),
                Description=skill.get("value"),
                Speciality=skill.get("type"),
            )
        )


def _hrflow_job_assign_name(hrflow_job: HrFlowJob, oracleorc_job: t.Dict) -> None:
    """
    Procedure to assign name of a `HrFlow.Job` from a `OracleORC.JobRequisition`

    Args:
      hrflow_job (HrFlowJob): pydantic object representing a HrFlow Job
      oracleorc_job (dict): dictionary representing the the Oracle ORC Job Requisition\
 to take the name from

    Returns:
      None
    """

    s = {
        oracleorc_job.get(key)
        for key in [
            "Keyword",
            "CorrectedKeyword",
            "SuggestedKeyword",
            "RequisitionId",
            "SiteNumber",
        ]
    }
    s.remove(None)

    hrflow_job.name = " - ".join(s)


def _hrflow_job_assign_location(hrflow_job: HrFlowJob, oracleorc_job: t.Dict) -> None:
    """
    Procedure to assign location to a `HrFlow.Job` from an `OracleORC.JobRequisition`

    Args:
      hrflow_job (HrFlowJob): pydantic object representing a HrFlow Job
      oracleorc_job (dict): dictionary representing the the Oracle ORC Job Requisition\
 to take the location from

    Returns:
      None
    """

    hrflow_job.location = Location(
        text=oracleorc_job.get("Location"),
        lat=oracleorc_job.get("Latitude"),
        lng=oracleorc_job.get("Longitude"),
        fields=dict(
            postcode=oracleorc_job.get("WorkLocationZipCode"),
            country=oracleorc_job.get("WorkLocationCountryCode"),
        ),
    )


def _hrflow_object_assign_metadatas_and_tags(
    target: t.Union[HrFlowJob, HrFlowProfile],
    source: t.Dict,
    mkeys: t.List[str],
    tkeys: t.List[str],
) -> None:
    """
    Procedure to assign `HrFlow.Metadata` and `HrFlow.Tag` objects to a `HrFlow.(Job|\
Profile)` from an `OracleORC.(JobRequisition|Candidate)`

    Args:
      target (HrFlow.Job | HrFlow.Profile): target object to assign metadatas and tags\
 to
      source (dictionnary): Oracle ORC Candidate or Job Requisition object represented\
 as a dict
      mkeys (list[str]): the keys of the `source` to use for metadatas
      tkeys (list[str]): the keys of the `source` to use for tags

    Returns:
      None
    """

    for field, keys in dict(tags=tkeys, metadatas=mkeys).items():
        setattr(
            target,
            field,
            [
                dict(
                    name=f"{'oracleorc_' if field=='tags' else ''}{key}",
                    value=str(source.get(key)),
                )
                for key in keys
                if source.get(key) is not None
            ],
        )


def _hrflow_job_assign_ranges_date(
    hrflow_job: HrFlowJob, oracleorc_job: t.Dict
) -> None:
    """
    Procedure to assign `HrFlow.RangesDate` to a `HrFlow.Job` from a `OracleOCR.\
JobRequisition`

    Args:
      hrflow_job (HrFlowJob): pydantic object representing a HrFlow Job
      oracleorc_job (dict): dictionary representing the the Oracle ORC Job Requisition\
 to take the date ranges from

    Returns:
      None
    """

    hrflow_job.ranges_date = [
        RangesDate(
            name="PostingDates",
            value_min=oracleorc_job.get("PostingStartDate"),
            value_max=oracleorc_job.get("PostingEndDate"),
        )
    ]


def _hrflow_profile_assign_info(
    hrflow_profile: HrFlowProfile, oracleorc_profile: t.Dict
) -> None:
    """
    Procedure to assign `HrFlow.ProfileInfo` to a `HrFlow.Profile` from an \
 `OracleORCCandidate`

    Args:
      hrflow_profile (HrFlow.Profile): pydantic object representing a HrFlow Profile
      oracleorc_profile (dict): dictionary representing the Oracle ORC Candidate to\
 take the info from

    Returns:
      None
    """

    hrflow_profile.info = ProfileInfo(
        full_name=oracleorc_profile.get("FullName"),
        first_name=oracleorc_profile.get("FirstName"),
        last_name=oracleorc_profile.get("LastName"),
        email=oracleorc_profile.get("Email"),
    )
