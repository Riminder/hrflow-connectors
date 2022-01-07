from typing import Optional, Dict, Any 
from pydantic import BaseModel

class SAPSuccessFactorsJobRequistion(BaseModel):
    annual_SA: str
    location: Optional[str]
    city : Optional[str] = None
    country : Optional[str] = None
    department : Optional[str] = None
    division : Optional[str] = None
    facility : Optional[str] = None
    function : Optional[str] = None
    industry : Optional[str] = None
    monthly_salary: Optional[str] = None
    salaryBase: Optional[str] = None
    otherBonus: Optional[str] = None
    salaryMax: Optional[str] = None
    salaryMin: Optional[str] = None
    stateProvince: Optional[str] = None
    jobStartDate: Optional[str] = None
    recruiterTeam: Optional[Dict[str, Any]] = None
    hiringManagerTeam: Optional[Dict[str, Any]] = None
    sourcerTeam: Optional[Dict[str, Any]] = None

class SAPSuccessFactorsJob(BaseModel):
    jobDescription : str
    jobTitle: str    
    jobReqId: str
    jobRequisition: SAPSuccessFactorsJobRequistion



