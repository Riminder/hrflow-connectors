import typing as t

from msgspec import Struct


class RecruiterFlowUser(Struct):
    email: str
    first_name: str
    id: int
    img_link: str
    last_name: str
    name: str


class RecruiterFlowCompanyBlockStatus(Struct):
    status_id: int
    status_name: str


class RecruiterFlowEducation(Struct):
    degree: str
    from_: t.List[int]
    rank: int
    school: str
    specialization: str
    to: t.List[t.Optional[int]]


class RecruiterFlowExperience(Struct):
    description: t.Optional[str]
    designation: str
    from_: t.List[int]
    linkedin_company_id: t.Optional[int]
    linkedin_company_profile_url: t.Optional[str]
    organization: str
    rank: int
    to: t.List[t.Optional[int]]


class RecruiterFlowFile(Struct):
    entities: t.List[t.Dict[str, t.Any]]
    file_category_id: int
    filename: str
    id: int
    is_primary: bool
    link: str
    permission: int
    upload_time: str


class RFJob(Struct):
    added_time: str
    added_to_job_by: RecruiterFlowUser
    client_company_id: int
    client_company_name: str
    department: t.Dict[str, t.Any]
    department_name: str
    disqualification_reason: t.Optional[str]
    highlight_note: t.Optional[str]
    interested: t.Optional[str]
    is_open: bool
    job_id: int
    job_status: t.Dict[str, t.Any]
    job_visibility_id: int
    name: str
    probability: int
    stage_moved: str
    stage_name: str
    starred: int
    title: str


class RecruiterFlowContact(Struct):
    icon: t.List[str]
    time: str
    type: t.Dict[str, t.Any]


class RecruiterFlowProfileLocation(Struct):
    city: str
    country: str
    google_place_id: t.Optional[str]
    location: str
    postal_code: t.Optional[str]
    state: str
    street_address_1: t.Optional[str]
    street_address_2: t.Optional[str]


class RecruiterFlowRating(Struct):
    added_by: RecruiterFlowUser
    id: int
    name: int


class RecruiterFlowStatus(Struct):
    id: t.Optional[int]
    name: t.Optional[str]


class RecruiterFlowProfile(Struct):
    added_by: t.Optional[RecruiterFlowUser]
    added_time: str
    angellist_profile: t.Optional[str]
    attributes: t.List[str]
    behance_profile: t.Optional[str]
    campaigns: t.List[str]
    candidate_summary: t.Optional[str]
    client: t.Optional[str]
    client_company_id: t.Optional[int]
    comm_status_id: t.Optional[int]
    company_block_status: RecruiterFlowCompanyBlockStatus
    current_designation: str
    current_organization: str
    custom_fields: t.List[str]
    do_not_email: bool
    dribbble_profile: t.Optional[str]
    education: t.List[RecruiterFlowEducation]
    education_details: t.List[RecruiterFlowEducation]
    email: t.List[str]
    experience: t.List[RecruiterFlowExperience]
    facebook_profile: t.Optional[str]
    files: t.List[RecruiterFlowFile]
    first_name: str
    github_profile: t.Optional[str]
    id: int
    img_link: str
    jobs: t.List[RFJob]
    last_contact: RecruiterFlowContact
    last_contact_type: int
    last_contacted: str
    last_engaged: str
    last_engagement: RecruiterFlowContact
    last_engagement_type: int
    last_name: str
    latest_activity_time: str
    lead_owner: RecruiterFlowUser
    linkedin_profile: str
    location: RecruiterFlowProfileLocation
    name: str
    notes: t.List[str]
    phone_number: t.List[str]
    prospect_id: int
    prospect_type_id: int
    rating: RecruiterFlowRating
    skills: t.List[str]
    source_name: str
    status: RecruiterFlowStatus
    submission_times: t.List[str]
    tags: t.List[str]
    twitter_profile: t.Optional[str]
    upcoming_activities: t.Dict[str, t.Any]
    xing_profile: t.Optional[str]


class RecruiterFlowFrequency(Struct):
    display_name: str
    id: int
    name: str


class RecruiterFlowCompany(Struct):
    id: int
    img_link: str
    name: str


class RecruiterFlowCustomField(Struct):
    id: int
    name: str
    value: t.Union[int, str]


class RecruiterFlowHiringTeam(Struct):
    email: str
    first_name: str
    img_link: str
    last_name: str
    name: str
    role: str
    role_id: int
    user_id: int


class RecruiterFlowJobStatus(Struct):
    color: str
    id: int
    name: str


class RecruiterFlowJobType(Struct):
    id: int
    name: str


class RecruiterFlowJobLocation(Struct):
    city: str
    country: str
    id: int
    iso_3166_1_alpha_2_code: str
    name: str
    postal_code: str
    state: t.Optional[str]
    zipcode: str


class RecruiterFlowPayRate(Struct):
    currency: str
    frequency: RecruiterFlowFrequency
    number: str


class RecruiterFlowWorkQuantum(Struct):
    frequency: RecruiterFlowFrequency
    is_full_time: bool
    number: str
    unit: RecruiterFlowFrequency


class RecruiterFlowSalary(Struct):
    currency: str
    number: int


class RecruiterFlowJob(Struct):
    about_position: str
    apply_link: str
    bill_rate: RecruiterFlowPayRate
    commission_rate: int
    company: RecruiterFlowCompany
    contract_end_date: str
    contract_start_date: str
    created_at: str
    current_opening: int
    custom_fields: t.List[RecruiterFlowCustomField]
    department: str
    employment_type: str
    expected_fee: RecruiterFlowSalary
    expected_salary: RecruiterFlowSalary
    experience_range_end: int
    experience_range_start: int
    files: t.List[RecruiterFlowFile]
    hiring_team: t.List[RecruiterFlowHiringTeam]
    id: int
    is_open: bool
    job_status: RecruiterFlowJobStatus
    job_type: RecruiterFlowJobType
    job_visibility_id: int
    last_opened: str
    locations: t.List[RecruiterFlowJobLocation]
    maildrop_email: str
    name: str
    number_of_openings: int
    pay_rate: RecruiterFlowPayRate
    publish_to_careers_page: bool
    salary_frequency: str
    salary_range_currency: str
    salary_range_end: int
    salary_range_start: int
    title: str
    work_quantum: RecruiterFlowWorkQuantum
