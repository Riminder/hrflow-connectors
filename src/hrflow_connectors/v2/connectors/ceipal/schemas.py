import typing as t

from msgspec import Struct


class PayRates(Struct):
    pay_rate: str
    pay_rate_pay_frequency_type: str
    pay_rate_employment_type: str
    pay_rate_currency: str


class JobPostingDetails(Struct):
    id: str
    job_code: str
    business_unit_id: int
    position_title: str
    address: str
    city: str
    postal_code: str
    assigned_recruiter: str
    created: str
    posted_by: str
    duration: str
    experience: str
    min_experience: str
    job_start_date: str
    job_end_date: str
    modified: str
    work_authorization: str
    number_of_positions: int
    closing_date: str
    remote_opportunities: str
    requisition_description: str
    public_job_desc: str
    public_job_title: str
    employment_type: str
    pay_rates: t.List[PayRates]
    primary_recruiter: str
    department: str
    currency: str
    job_status: str
    industry: str
    tax_terms: str
    skills: str
    country: str
    state: str
    apply_job: str
    apply_job_without_registration: str
    contact_person: str
    posted: str


class Applicant(Struct):
    id: str
    applicant_id: str
    firstname: str
    lastname: str
    middlename: str
    consultant_name: str
    email: str
    email_address_1: str
    other_phone: str
    address: str
    city: str
    state: str
    country: str
    mobile_number: str
    created_at: str
    created_by: str
    applicant_status: str
    skills: str
    source: str
    resume_path: str
    home_phone_number: str
    work_phone_number: str
    job_title: str


class Document(Struct):
    title: str
    doc_title: str
    created_by: str
    created_on: str
    modified_by: str


class ApplicantDetails(Struct):
    id: str
    applicant_id: str
    firstname: str
    lastname: str
    middlename: str
    consultant_name: str
    email: str
    email_address_1: str
    other_phone: str
    address: str
    city: str
    state: str
    country: str
    mobile_number: str
    created_at: str
    created_by: str
    documents: t.List[Document]
    resume_path: str
    source: str
    home_phone_number: str
    work_phone_number: str
