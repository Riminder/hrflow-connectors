from typing import Optional, Union, List
from pydantic import BaseModel, Field


class MonsterProfile(BaseModel):
    City: str = Field(..., Description="City of the profile")
    CountryCode: str = Field(..., Description="CountryCode of the profile")
    EmailAddress: str = Field(..., Description="Email address of the profile")
    FileContents: List[int] = Field(
        ...,
        Description="Content of the resume of the profile on the form of a list of int",
    )
    FileExt: str = Field(
        ...,
        Description="This contains the resume in its original format as it was made available "
        "by the applicant. It is a C#, unsigned Byte Array. If the resume was "
        "created interactively on Monster, the resume is provided as consistently "
        "formatted HTML. If an applicant originally uploaded a resume to Monster ("
        "e.g., as a PDF), it is delivered in the original document format. Resume "
        "size can be up to 10 MB. Due to some additional data and overhead coming "
        "from the serialization / deserialization process, XML format should be set "
        "to accept up to 8 MB/ JSON format should be set to accept up to 20 MB.",
    )
    FirstName: str = Field(..., Description="First name of the profile")
    JobRefID: str = Field(..., Description="Reference of the profile")
    LastName: str = Field(..., Description="Last name of the profile")
    PhoneNumber: str = Field(..., Description="Phone number of the profile")
    ResumeValue: str = Field(
        ..., Description="This is a unique alpha numeric value on the Monster side."
    )
    State: str
    VendorField: str = Field(..., Description="Commentary from the job")
    WorkAuthorization: int = Field(
        ...,
        Description="This is a value of whether the applicant is authorized to work "
        "in the country provided in the countryCode",
    )
    ZIPCode: str = Field(..., Description="ZIP code of the profile")
