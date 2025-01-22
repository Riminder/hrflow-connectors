import base64
import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct, field
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.sapsuccessfactors.schemas import (
    SapCandidateModel,
    SAPSuccessFactorsJobRequistion,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

SAP_JOBS_ENDPOINT_LIMIT = 100

SAP_PRODUCTION_URL = "https://{api_server}/odata/v2"

job_expanded_fields = [
    "jobProfile",
    "division_obj",
    "legalEntity_obj",
    "status",
    "jobReqLocale",
    "jobRoleEntity",
    "recruiterTeamGroup",
    "hiringManagerTeamGroup",
    "sourcerTeamGroup",
    "experienceReq",
    "location_obj",
    "competencies",
    "department_obj",
]

profile_expanded_fields = [
    "education",
    "insideWorkExperience",
    "outsideWorkExperience",
    "resume",
]


class AuthParameters(Struct):
    api_server: Annotated[
        str,
        Meta(
            description="Server to be accessed",
        ),
    ]
    api_key: Annotated[
        str,
        Meta(
            description="API Key used to authenticate on the SAP API",
        ),
    ]


class WriteProfilesParameters(Struct):
    pass


class BaseReadParameters(Struct, omit_defaults=True):
    top: Annotated[
        t.Optional[int],
        Meta(
            description="Show only the first n items value is capped at {}".format(
                SAP_JOBS_ENDPOINT_LIMIT
            ),
        ),
    ] = field(default=SAP_JOBS_ENDPOINT_LIMIT, name="$top")
    skip: Annotated[
        t.Optional[int],
        Meta(
            description="Skip the first n items",
        ),
    ] = field(default=None, name="$skip")
    search: Annotated[
        t.Optional[str],
        Meta(
            description="Search items by search phrases",
        ),
    ] = field(default=None, name="$search")
    filter: Annotated[
        t.Optional[str],
        Meta(
            description="Filter items by property values",
        ),
    ] = field(default=None, name="$filter")
    count: Annotated[
        t.Optional[bool],
        Meta(
            description="Include count of items",
        ),
    ] = field(default=None, name="$count")


class ReadProfilesParameters(BaseReadParameters):
    orderby: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description=(
                "Order items by property values\nAvailable values : address, address"
                " desc, address2, address2 desc, agreeToPrivacyStatement,"
                " agreeToPrivacyStatement desc, anonymized, anonymized desc,"
                " anonymizedDateTime, anonymizedDateTime desc, candidateId, candidateId"
                " desc, candidateLocale, candidateLocale desc, cellPhone, cellPhone"
                " desc, city, city desc, contactEmail, contactEmail desc, country,"
                " country desc, creationDateTime, creationDateTime desc, currentTitle,"
                " currentTitle desc, dataPrivacyId, dataPrivacyId desc,"
                " dateofAvailability, dateofAvailability desc, externalCandidate,"
                " externalCandidate desc, firstName, firstName desc, homePhone,"
                " homePhone desc, lastLoginDateTime, lastLoginDateTime desc,"
                " lastModifiedDateTime, lastModifiedDateTime desc, lastName, lastName"
                " desc, middleName, middleName desc, partnerMemberId, partnerMemberId"
                " desc, partnerSource, partnerSource desc, password, password desc,"
                " primaryEmail, primaryEmail desc, privacyAcceptDateTime,"
                " privacyAcceptDateTime desc, publicIntranet, publicIntranet desc,"
                " shareProfile, shareProfile desc, usersSysId, usersSysId desc,"
                " visibilityOption, visibilityOption desc, zip, zip desc"
            ),
        ),
    ] = field(default=None, name="$orderby")


class ReadJobsParameters(BaseReadParameters):
    orderby: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description=(
                "Order items by property values\nAvailable values : accommodation_cost,"
                " accommodation_cost desc, age, age desc, agency_fee, agency_fee desc,"
                " annual_FB, annual_FB desc, annual_PF, annual_PF desc, annual_SA,"
                " annual_SA desc, annual_cashPay, annual_cashPay desc, annual_gratuity,"
                " annual_gratuity desc, annual_retirals, annual_retirals desc,"
                " appTemplateId, appTemplateId desc, assessRatingScaleName,"
                " assessRatingScaleName desc, candidateProgress, candidateProgress"
                " desc, city, city desc, classificationTime, classificationTime desc,"
                " classificationType, classificationType desc, closedDateTime,"
                " closedDateTime desc, comment, comment desc, commentEquip,"
                " commentEquip desc, commission, commission desc, corporatePosting,"
                " corporatePosting desc, costCenterId, costCenterId desc, costOfHire,"
                " costOfHire desc, country, country desc, createdDateTime,"
                " createdDateTime desc, currency, currency desc, customString10,"
                " customString10 desc, customString3, customString3 desc,"
                " customString4, customString4 desc, customString8, customString8 desc,"
                " defaultLanguage, defaultLanguage desc, deleted, deleted desc,"
                " department, department desc, division, division desc, erpAmount,"
                " erpAmount desc, facility, facility desc, formDataId, formDataId desc,"
                " formDueDate, formDueDate desc, function, function desc,"
                " hiringManagerNote, hiringManagerNote desc, industry, industry desc,"
                " instrCostHire, instrCostHire desc, instrERS, instrERS desc,"
                " instrEquest, instrEquest desc, instrEquipment, instrEquipment desc,"
                " instrInterview, instrInterview desc, instrInvolvedParties,"
                " instrInvolvedParties desc, instrManagernote, instrManagernote desc,"
                " instrOverallComments, instrOverallComments desc, instrPD, instrPD"
                " desc, instrReq, instrReq desc, instrReqDates, instrReqDates desc,"
                " instrReqDetails, instrReqDetails desc, instrReqJustify,"
                " instrReqJustify desc, instrReqRequire, instrReqRequire desc,"
                " instrSalary, instrSalary desc, instr_rec_cost, instr_rec_cost desc,"
                " internalStatus, internalStatus desc, intranetPosting, intranetPosting"
                " desc, isDraft, isDraft desc, jobCode, jobCode desc, jobReqGUId,"
                " jobReqGUId desc, jobReqId, jobReqId desc, jobRole, jobRole desc,"
                " jobStartDate, jobStartDate desc, lastModifiedBy, lastModifiedBy desc,"
                " lastModifiedDateTime, lastModifiedDateTime desc,"
                " lastModifiedProxyUserId, lastModifiedProxyUserId desc, legentity,"
                " legentity desc, location, location desc, misc_cost, misc_cost desc,"
                " monthly_FB, monthly_FB desc, monthly_PF, monthly_PF desc, monthly_SA,"
                " monthly_SA desc, monthly_cashPay, monthly_cashPay desc,"
                " monthly_gratuity, monthly_gratuity desc, monthly_retirals,"
                " monthly_retirals desc, monthly_salary, monthly_salary desc,"
                " numberOpenings, numberOpenings desc, openingsFilled, openingsFilled"
                " desc, otherBonus, otherBonus desc, otherCompensation,"
                " otherCompensation desc, otherEquip, otherEquip desc, othrComms,"
                " othrComms desc, overallScaleName, overallScaleName desc,"
                " positionNumber, positionNumber desc, postalcode, postalcode desc,"
                " ratedApplicantCount, ratedApplicantCount desc, recruitJust,"
                " recruitJust desc, replforwhom, replforwhom desc,"
                " restorehiringManagerTeamAdminDefaults,"
                " restorehiringManagerTeamAdminDefaults desc,"
                " restorerecruiterTeamAdminDefaults, restorerecruiterTeamAdminDefaults"
                " desc, restoresourcerTeamAdminDefaults,"
                " restoresourcerTeamAdminDefaults desc,"
                " restorevpOfStaffingTeamAdminDefaults,"
                " restorevpOfStaffingTeamAdminDefaults desc, reverseScale, reverseScale"
                " desc, salRateType, salRateType desc, salaryBase, salaryBase desc,"
                " salaryComment, salaryComment desc, salaryMax, salaryMax desc,"
                " salaryMin, salaryMin desc, stateProvince, stateProvince desc,"
                " statusSetId, statusSetId desc, stockPackage, stockPackage desc,"
                " targetBonusAmount, targetBonusAmount desc, tempDate, tempDate desc,"
                " templateId, templateId desc, templateType, templateType desc,"
                " timeToFill, timeToFill desc, total_earnings, total_earnings desc,"
                " total_fixed_pay, total_fixed_pay desc, total_hire_cost,"
                " total_hire_cost desc, travel_cost, travel_cost desc, workHours,"
                " workHours desc"
            ),
        ),
    ] = field(default=None, name="$orderby")


# enriches a job object with it's requisition field
def enrich_requisition(parameters: AuthParameters, job: t.Dict) -> t.Dict:
    job_and_requisition = dict()
    job_and_requisition["job"] = job

    url = job.get("jobRequisition", {}).get("__deferred", {}).get("uri")
    requisition = requests.get(
        url, headers={"APIKey": parameters.api_key, "Accept": "application/json"}
    )
    requisition = requisition.json()
    job_and_requisition["requisition"] = requisition.get("d")
    return job_and_requisition


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    # Set the parameters for the API call
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    params["$expand"] = ",".join(job_expanded_fields)

    url = (
        SAP_PRODUCTION_URL.format(api_server=auth_parameters.api_server)
        + "/JobRequisition"
    )

    # url = "https://sandbox.api.sap.com/successfactors/odata/v2" + "/JobRequisition"

    response = requests.get(
        url,
        headers={
            "APIKey": auth_parameters.api_key,
            "Accept": "application/json",  # Request JSON instead of XML (default)
        },
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull jobs from SAP, status_code={}".format(response.status_code)
        )
        raise Exception("Failed to pull jobs from SAP")
    response = response.json()
    jobs = response.json()["d"]["results"]

    for job in jobs:
        yield job


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    # Set the API endpoint URL
    endpoint_url = (
        SAP_PRODUCTION_URL.format(api_server=auth_parameters.api_server) + "/Candidate"
    )

    # Set the headers with the API key
    headers = {
        "APIKey": auth_parameters.api_key,
        "Accept": "application/json",  # Request JSON instead of XML (default)
    }

    # Set the parameters for the API call
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    params["$expand"] = ",".join(profile_expanded_fields)

    # Make the GET request
    response = requests.get(endpoint_url, headers=headers, params=params)

    # Check if the request was successful
    if response.status_code != 200:
        adapter.error(f"Failed to retrieve candidates. Response: {response.text}")
        raise Exception(f"Failed to retrieve candidates. Response: {response.text}")

    # Extract the candidate data from the response
    candidates = response.json()["d"]["results"]

    for candidate in candidates:
        # Retrieve the resume
        resume = dict()
        resume_url = candidate["resume"]["__deferred"]["uri"]
        resume_response = requests.get(resume_url, headers=headers)
        file_content = resume_response.json()["d"]["fileContent"]
        resume["raw"] = base64.b64decode(file_content)
        resume["content_type"] = resume_response.json()["d"]["mimeType"]
        # Retrieve the tags
        tags_url = candidate["tags"]["__deferred"]["uri"]
        tags_response = requests.get(tags_url, headers=headers)
        tags = tags_response.json()["d"]["results"]
        candidate["resume"] = resume
        candidate["tags"] = tags
        yield candidate


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    for profile in items:
        response = requests.post(
            SAP_PRODUCTION_URL.format(api_server=auth_parameters.api_server)
            + "/Candidate",
            headers={"APIKey": auth_parameters.api_key, "Accept": "application/json"},
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to SAP status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


def update_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    for profile in items:
        candidate_id = profile.pop("candidateId")
        response = requests.put(
            SAP_PRODUCTION_URL.format(api_server=auth_parameters.api_server)
            + "/Candidate({})".format(candidate_id),
            headers={
                "APIKey": auth_parameters.api_key,
                "Accept": "application/json",  # Request JSON instead of XML (default)
            },
            json=profile,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to SAP status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=SapCandidateModel,
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters, update=WriteProfilesParameters
        ),
        function=merge(
            create=write_profiles,
            update=update_profiles,
        ),
    ),
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read_profiles,
            update=read_profiles,
            archive=read_profiles,
        ),
    ),
)

JobsAisle = Aisle(
    name=Entity.job,
    schema=SAPSuccessFactorsJobRequistion,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=read_jobs,
            update=read_jobs,
            archive=read_jobs,
        ),
    ),
)
