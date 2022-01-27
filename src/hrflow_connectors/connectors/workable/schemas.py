from pydantic import BaseModel
from typing import Optional


class WorkableJobModel(BaseModel):
    title: Optional[str]
    shortcode: Optional[str]
    country: Optional[str]
    state: Optional[str]
    city: Optional[str]
    department: Optional[str]
    published_on: Optional[str]
    url: Optional[str]
    application_url: Optional[str]
    created_at: Optional[str]
    description: Optional[str]
    employment_type: Optional[str]
    industry: Optional[str]
    function: Optional[str]
    experience: Optional[str]
    education: Optional[str]
