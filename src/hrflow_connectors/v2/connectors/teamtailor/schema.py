from typing import Any, List, Optional

from msgspec import Struct, field


class TeamtailorJobAttributes(Struct):
    title: str
    pitch: Optional[str]
    body: str
    created_at: str = field(name="created-at")
    updated_at: Optional[str] = field(name="updated-at")
    status: Optional[str]
    human_status: Optional[str] = field(name="human-status")
    language_code: Optional[str] = field(name="language-code")
    picture: Optional[str]
    external_application_url: Optional[str] = field(name="external-application-url")
    tags: List[str]
    remote_status: Optional[str] = field(name="remote-status")
    currency: Optional[str]
    salary_time_unit: Optional[str] = field(name="salary-time-unit")
    min_salary: Optional[int] = field(name="min-salary")
    max_salary: Optional[int] = field(name="max-salary")
    employment_type: Optional[str] = field(name="employment-type")
    employment_level: Optional[str] = field(name="employment-level")
    sharing_image_layout: Optional[str] = field(name="sharing-image-layout")
    template_name: Optional[str] = field(name="template-name")
    name_requirement: Optional[str] = field(name="name-requirement")
    resume_requirement: Optional[str] = field(name="resume-requirement")
    additional_files_requirement: Optional[str] = field(
        name="additional-files-requirement"
    )
    cover_letter_requirement: Optional[str] = field(name="cover-letter-requirement")
    phone_requirement: Optional[str] = field(name="phone-requirement")
    internal: Optional[bool]
    internal_name: Optional[str] = field(name="internal-name")
    pinned: Optional[bool]
    start_date: Optional[str] = field(name="start-date")
    end_date: Optional[str] = field(name="end-date")


class TeamtailorJob(Struct):
    attributes: TeamtailorJobAttributes
    links: Optional[dict]
    id: Any
    relationships: Optional[dict]
    type: str


class TeamtailorCandidateAttributes(Struct):
    first_name: str = field(name="first-name")
    last_name: str = field(name="last-name")
    created_at: str = field(name="created-at")
    updated_at: Optional[str] = field(name="updated-at")
    email: str
    phone: Optional[str]
    picture: Optional[str]
    linkedin_url: Optional[str] = field(name="linkedin-url")
    linkedin_uid: Optional[str] = field(name="linkedin-uid")
    linkedin_profile: Optional[str] = field(name="linkedin-profile")
    facebook_profile: Optional[str] = field(name="facebook-profile")
    facebook_id: Optional[str] = field(name="facebook-id")
    pitch: Optional[str]
    resume: Optional[str]
    original_resume: Optional[str] = field(name="original-resume")
    sourced: Optional[bool]
    connected: Optional[bool]
    internal: Optional[bool]
    referred: Optional[bool]
    referring_url: Optional[str] = field(name="referring-url")
    referring_site: Optional[str] = field(name="referring-site")
    unsubscribed: Optional[bool]
    tags: Optional[List[str]]


class TeamtailorCandidate(Struct):
    attributes: TeamtailorCandidateAttributes
    links: Optional[dict]
    id: str
    relationships: Optional[dict]
    type: str
