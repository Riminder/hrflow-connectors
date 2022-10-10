from typing import Optional

from pydantic import BaseModel


class CeridianDayforceJobModel(BaseModel):
    Title: str
    Description: Optional[str]
    ClientSiteName: Optional[str]
    ClientSiteXRefCode: Optional[str]
    CompanyName: Optional[str]
    ParentCompanyName: Optional[str]
    JobDetailsUrl: str
    ApplyUrl: Optional[str]
    AddressLine1: Optional[str]
    City: Optional[str]
    State: Optional[str]
    Country: Optional[str]
    PostalCode: Optional[str]
    DatePosted: Optional[str]
    LastUpdated: Optional[str]
    ReferenceNumber: int
    ParentRequisitionCode: Optional[int]
    IsVirtualLocation: Optional[bool]
