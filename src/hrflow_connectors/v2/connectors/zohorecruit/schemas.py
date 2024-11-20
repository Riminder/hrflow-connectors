import typing as t
from enum import Enum

from pydantic import BaseModel, Field


class ZohoUser(BaseModel):
    name: str
    id: str


class ZohoRecruiter(BaseModel):
    name: str
    id: str
    email: str
    photoSrc: str


class ZohoState(str, Enum):
    DRAFT = "draft"
    SAVE = "save"


class Approval(BaseModel):
    delegate: bool
    approve: bool
    reject: bool
    resubmit: bool


class JobOpening(BaseModel):
    id: str
    Posting_Title: t.Optional[str]
    Client_Name: str
    Job_Opening_Name: str
    Job_Description: t.Optional[str]
    Job_Type: t.Optional[str]
    Job_Opening_Status: t.Optional[str]
    Job_Opening_ID: t.Optional[str]
    currency_symbol: t.Optional[str] = Field(alias="$currency_symbol")
    Required_Skills: t.Optional[str]
    Industry: t.Optional[str]
    state: t.Optional[ZohoState] = Field(alias="$state")
    process_flow: t.Optional[bool] = Field(alias="$process_flow")
    City: t.Optional[str]
    State: t.Optional[str]
    Zip_Code: t.Optional[str]
    Country: t.Optional[str]
    approved: t.Optional[bool] = Field(alias="$approved")
    Created_Time: t.Optional[str]
    Modified_Time: t.Optional[str]
    Date_Opened: t.Optional[str]
    Target_Date: t.Optional[str]
    Last_Activity_Time: t.Optional[str]
    approval: t.Optional[Approval] = Field(alias="$approval")
    approval_state: t.Optional[str] = Field(alias="$approval_state")
    Remote_Job: t.Optional[bool]
    followed: t.Optional[bool] = Field(alias="$followed")
    editable: t.Optional[bool] = Field(alias="$editable")
    Is_Locked: t.Optional[bool]
    Salary: t.Optional[str]
    Work_Experience: t.Optional[str]
    Number_of_Positions: t.Optional[str]
    Associated_Tags: t.Optional[t.List[str]]
    Account_Manager: t.Optional[ZohoUser]
    Assigned_Recruiter: t.Optional[t.List[ZohoRecruiter]]
    Assigned_Recruiters: t.Optional[t.List[ZohoRecruiter]]
    Contact_Name: t.Optional[ZohoUser]
    Created_By: t.Optional[ZohoUser]
    Modified_By: t.Optional[ZohoUser]
    Is_Attachment_Present: t.Optional[bool]
    Keep_on_Career_Site: t.Optional[bool]
    No_of_Candidates_Hired: t.Optional[int]
    Expected_Revenue: t.Optional[float]
    Is_Hot_Job_Opening: t.Optional[bool]
    Publish: t.Optional[bool]
    Actual_Revenue: t.Optional[float]
    Missed_Revenue: t.Optional[float]
    No_of_Candidates_Associated: t.Optional[int]
    Revenue_per_Position: t.Optional[float]


class ZohoDuration(BaseModel):
    from_: str = Field(alias="from")
    to: t.Optional[str]


class Experience_Detail(BaseModel):
    id: str
    Company: str
    I_currently_work_here: bool
    Summary: str
    Work_Duration: ZohoDuration
    Occupation_Title: str


class Educational_Detail(BaseModel):
    id: str
    Institute_School: str
    Currently_pursuing: bool
    Degree: str
    Major_Department: str
    Duration: ZohoDuration


class Candidate(BaseModel):
    id: str
    First_Name: t.Optional[str]
    Last_Name: str
    Full_Name: t.Optional[str]
    Salutation: t.Optional[str]
    Email: t.Optional[str]
    Phone: t.Optional[str]
    Mobile: t.Optional[str]
    Fax: t.Optional[str]
    Street: t.Optional[str]
    City: t.Optional[str]
    State: t.Optional[str]
    Zip_Code: t.Optional[str]
    Country: t.Optional[str]
    Current_Job_Title: t.Optional[str]
    Current_Salary: t.Optional[int]
    Expected_Salary: t.Optional[int]
    Current_Employer: t.Optional[str]
    Experience_in_Years: t.Optional[int]
    Experience_Details: t.Optional[t.List[Experience_Detail]]
    Educational_Details: t.Optional[t.List[Educational_Detail]]
    Highest_Qualification_Held: t.Optional[str]
    Skill_Set: t.Optional[str]
    Created_Time: t.Optional[str]
    Last_Activity_Time: t.Optional[str]
    Updated_On: t.Optional[str]
    Last_Mailed_Time: t.Optional[str]
    Source: t.Optional[str]
    Origin: t.Optional[str]
    currency_symbol: t.Optional[str] = Field(alias="$currency_symbol")
    whatsapp_available: t.Optional[bool] = Field(alias="$whatsapp_available")
    state: t.Optional[ZohoState] = Field(alias="$state")
    converted: t.Optional[bool] = Field(alias="$converted")
    process_flow: t.Optional[bool] = Field(alias="$process_flow")
    approved: t.Optional[bool] = Field(alias="$approved")
    approval: t.Optional[str] = Field(alias="$approval")
    approval_state: t.Optional[str] = Field(alias="$approval_state")
    Candidate_Status: t.Optional[str]
    Candidate_ID: t.Optional[str]
    Candidate_Owner: t.Optional[ZohoUser]
    followed: t.Optional[bool] = Field(alias="$followed")
    LinkedIn__s: t.Optional[str]
    editable: t.Optional[bool] = Field(alias="$editable")
    Is_Locked: t.Optional[bool]
    Is_Unqualified: t.Optional[bool]
    Associated_Tags: t.Optional[t.List[str]]
    Additional_Info: t.Optional[str]
    Created_By: t.Optional[ZohoUser]
    Modified_By: t.Optional[ZohoUser]
    Secondary_Email: t.Optional[str]
    Is_Attachment_Present: t.Optional[bool]
    Rating: t.Optional[int]
    applied_with_linkedin: t.Optional[bool] = Field(alias="$applied_with_linkedin")
    No_of_Applications: t.Optional[int]
    Website: t.Optional[str]
    Twitter: t.Optional[str]
    Facebook__s: t.Optional[str]
    social_profiles: t.Optional[str] = Field(alias="$social_profiles")
    Skype_ID: t.Optional[str]
    Candidate_Stage: t.Optional[str]
    Fresh_Candidate: t.Optional[bool]
    Email_Opt_Out: t.Optional[bool]
    converted_detail: t.Optional[t.Dict] = Field(alias="$converted_detail")
    Career_Page_Invite_Status: t.Optional[str]
    Associated_any_Social_Profiles: t.Optional[bool]
