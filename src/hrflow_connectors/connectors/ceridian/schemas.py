from pydantic import BaseModel


class CeridianDayforceJobModel(BaseModel):
    Title: str
    Description: str
    ClientSiteName: str
    ClientSiteXRefCode: str
    CompanyName: str
    ParentCompanyName: str
    JobDetailsUrl: str
    ApplyUrl: str
    AddressLine1: str
    City: str
    State: str
    Country: str
    PostalCode: str
    DatePosted: str
    LastUpdated: str
    ReferenceNumber: int
    ParentRequisitionCode: int
    IsVirtualLocation: bool
