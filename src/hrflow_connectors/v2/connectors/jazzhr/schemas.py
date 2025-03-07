from pydantic import BaseModel


class JazzHrJobs(BaseModel):
    title: str
    hiring_lead_id: str
    employment_type: str
    minimum_experience: str
    description: str
    country: str
    confidential: str
    private: str
    department: str
    job_status: str
    state: str
    city: str
    postal_code: str
    syndication: str
    workflow_id: str
    canned_address: str
    canned_cover_letter: str
    canned_references: str
    canned_wmyu: str
    canned_linked_in: str
    canned_website: str
    canned_twitter_username: str
    canned_start: str
    canned_weekends: str
    canned_evenings: str
    canned_overtime: str
    canned_languages: str
    canned_salary: str
    canned_referral: str
    canned_license: str
    canned_cdl: str
    canned_relocate: str
    canned_citizen: str
    canned_education: str
    canned_college: str
    canned_gpa: str
    canned_over18: str
    canned_flighthours: str
    canned_flightgrade: str
    canned_felony: str
    canned_felonyexplain: str
    custom_questions_id: str
    internal_job_code: str
    eeo_1_job_category: str
    approved_salary_range_minimum: float
    approved_salary_range_maximum: float
    job_notes: str
    open_date: str


class JazzHrApplicants(BaseModel):
    first_name: str
    last_name: str
    email: str
    apply_date: str
    address: str
    city: str
    state: str
    postal: str
    phone: str
    job: str
    workflow_step_id: str
    coverletter: str
    source: str
    referral: str
    license: str
    cdl: str
    relocate: str
    citizen: str
    college: str
    gpa: str
    over18: str
    flighthours: str
    flightgrade: str
    linkedin: str
    twitter: str
    website: str
    languages: str
    salary: str
    start: str
    weekends: str
    evenings: str
    overtime: str
    felony: str
    felonyexplain: str
    wmyu: str
    references: str
    eeo_gender: str
    eeo_race: str
    eeo_disability: str
    eeoc_veteran: str
    eeoc_disability: str
    eeoc_disability_signature: str
    eeoc_disability_date: str
    resumetext: str
    base64_resume: str
