from pydantic import BaseModel


class WorkableJobModel(BaseModel):
    title: str
    shortcode: str
    country: str
    state: str
    city: str
    department: str
    published_on: str
    url: str
    application_url: str
    created_at: str
    description: str
    employment_type: str
    industry: str
    function: str
    experience: str
    education: str
