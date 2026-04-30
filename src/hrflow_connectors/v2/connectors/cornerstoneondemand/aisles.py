import enum
import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from msgspec.structs import asdict
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.cornerstoneondemand.schema import (
    CornerstoneApplicant,
    CornerstoneJobRequisition,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

Page_size_limit = 100

BASE_URL_MAP = {
    "stage": "https://{corpname}-stg.csod.com/services/api",
    "pilot": "https://{corpname}-pil.csod.com/services/api",
    "production": "https://{corpname}.csod.com/services/api",
}


class CornerstoneOnDemandEnvironment(str, enum.Enum):
    STAGE = "stage"
    PILOT = "pilot"
    PRODUCTION = "production"


class AuthParameters(Struct):
    corporation_name: Annotated[
        str,
        Meta(
            description=(
                "The name of the corporation that is registered in Cornerstone"
                " OnDemand."
            ),
        ),
    ]
    client_id: Annotated[
        str,
        Meta(
            description=(
                "The client ID of the application that is registered in Cornerstone"
                " OnDemand."
            ),
        ),
    ]
    client_secret: Annotated[
        str,
        Meta(
            description=(
                "The client secret of the application that is registered in Cornerstone"
                " OnDemand."
            ),
        ),
    ]
    environment: Annotated[
        CornerstoneOnDemandEnvironment,
        Meta(
            description=(
                "The environment in which the application is registered in Cornerstone"
                " OnDemand. Possible values are 'stage', 'pilot', and 'production'."
            ),
        ),
    ]


class ReadJobRequisitionsParameters(Struct, omit_defaults=True):
    Title: Annotated[
        t.Optional[str],
        Meta(
            description="Job Title.",
        ),
    ] = None
    ReqId: Annotated[
        t.Optional[str],
        Meta(
            description="Requisition ID.",
        ),
    ] = None
    FromDate: Annotated[
        t.Optional[str],
        Meta(
            description="UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss",
        ),
    ] = None
    ToDate: Annotated[
        t.Optional[str],
        Meta(
            description="UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss",
        ),
    ] = None
    lastModifiedSince: Annotated[
        t.Optional[str],
        Meta(
            description="UTC Datetime value. Format should be yyyy-mm-ddThh:mm:ss",
        ),
    ] = None
    DivisionId: Annotated[
        t.Optional[str],
        Meta(
            description="Division ID.",
        ),
    ] = None
    LocationId: Annotated[
        t.Optional[str],
        Meta(
            description="Location ID.",
        ),
    ] = None
    Statuses: Annotated[
        t.Optional[str],
        Meta(
            description=(
                'Comma separated list of statuses. e.g. "Draft,Open,Closed".\nEnum:'
                ' "Draft" "Open" "Closed" "Cancelled" "Pending Approval" "Approval'
                ' Denied" "Open - Pending Re - Approval" "On Hold"'
            )
        ),
    ] = None
    Language: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Language should include ISO language code. Example en-US, fr-FR,"
                " it-IT, en-GB..."
            ),
        ),
    ] = None
    limit: Annotated[
        t.Optional[int],
        Meta(
            description="The maximum number of records to return.",
        ),
    ] = 100


class Status(str, enum.Enum):
    New_Submission = "New Submission"
    In_Review = "In Review"
    Phone_Screening = "Phone Screening"
    Interview = "Interview"
    Background_Check = "Background Check"
    Offer_Letter = "Offer Letter"
    Closed = "Closed"
    Hired = "Hired"


class ReadApplicantsParameters(Struct, omit_defaults=True):
    CurrentStatus: Annotated[
        str,
        Meta(
            description="Valid Status. Enum: {}".format(
                [status.value for status in Status]
            )
        ),
    ]
    StatusDate: Annotated[
        t.Optional[str],
        Meta(
            description="Local Datetime Value. Format should be yyyy-mm-dd",
        ),
    ] = None
    CsodGUID: Annotated[
        t.Optional[str],
        Meta(
            description="Applicant's GUID Value.",
        ),
    ] = None
    FirstName: Annotated[
        t.Optional[str],
        Meta(
            description="Applicant's First Name.",
        ),
    ] = None
    LastName: Annotated[
        t.Optional[str],
        Meta(
            description="Applicant's Last Name.",
        ),
    ] = None
    RequisitionID: Annotated[
        t.Optional[str],
        Meta(
            description="Job Requisition Id.",
        ),
    ] = None
    JobTitle: Annotated[
        t.Optional[str],
        Meta(
            description="Job Requision Title.",
        ),
    ] = None
    limit: Annotated[
        t.Optional[int],
        Meta(
            description="The maximum number of records to return.",
        ),
    ] = 100


class Source(Struct):
    submissionSource: Annotated[
        str,
        Meta(
            description=(
                "How the application was submitted. If a custom submission source does"
                " not exist in CSOD, it will be added automatically to Admin settings."
                ' If the submission source is left empty, the source name, "Candidate'
                ' API" will be used.'
            ),
        ),
    ]
    submissionSourceName: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "A user-friendly display name showing how the application was"
                " submitted. If not specified, this will be the same as"
                " submissionSource."
            ),
        ),
    ] = None


class QuestionType(str, enum.Enum):
    Disclaimer = "Disclaimer"
    Compliance = "Compliance"
    Prescreening = "Prescreening"


class Question(Struct):
    type: Annotated[
        QuestionType,
        Meta(
            description=(
                "Enum: {}\nType of question. This"
                " is an enum of: Disclaimer, Compliance, Prescreening.".format(
                    [type.value for type in QuestionType]
                )
            ),
        ),
    ]
    id: Annotated[
        str,
        Meta(
            description="This value will be the internal id for each question type.",
        ),
    ]
    response: Annotated[
        t.Optional[str],
        Meta(
            description="The candidate’s response to the question.",
        ),
    ] = None


class WriteCandidatesParameters(Struct):
    jobRequisitionId: Annotated[
        str,
        Meta(
            description=(
                'The ATS job requisition’s identifier. This is a "ref" value and not'
                ' the internal "id". A correct example is Req123.'
            ),
        ),
    ]
    futureOpportunityOptIn: Annotated[
        t.Optional[bool],
        Meta(
            description="Candidate’s decision to be considered for other positions.",
        ),
    ] = None
    source: Annotated[
        t.Optional[Source],
        Meta(
            description=(
                "How the application was submitted. If this object is null, we will"
                " default the submissionSource to Candidate API."
            ),
        ),
    ] = None
    questions: Annotated[
        t.Optional[t.List[Question]],
        Meta(
            description=(
                "A collection of application submission data for questions of type:"
                " Disclaimer, Compliance and Prescreening."
            ),
        ),
    ] = None
    sendEmail: Annotated[
        bool,
        Meta(
            description=(
                "Default: true\nThis is an optional field allowing an API consumer to"
                " specify if the candidate should or should not receive an email upon"
                " successful submission of their application. When set sendEmail is"
                " true, the applicant will receive the apply as guest, or standard"
                " application submission email. This will be dependent on whether the"
                " candidate's profile has or has not already been claimed. Some clients"
                " also require applicants to claim their email address before"
                " successfully claiming their account. This workflow will remain the"
                " same. If left empty, this property will default to true."
            ),
        ),
    ] = True


def get_access_token(adapter: LoggerAdapter, auth_parameters: AuthParameters) -> str:
    auth_url = BASE_URL_MAP[auth_parameters.environment].format(
        corpname=auth_parameters.corporation_name
    )

    headers = {
        "Content-Type": "application/json",
        "cache-control": "no-cache",
    }

    payload = {
        "clientId": auth_parameters.client_id,
        "clientSecret": auth_parameters.client_secret,
        "grantType": "client_credentials",
        "scope": (
            "jobrequisition:read jobapplicant:read jobapplication:create"
            " jobapplicant:update"
        ),
    }
    response = requests.post(auth_url, headers=headers, json=payload)
    if response.status_code != 200:
        adapter.error(
            "Failed to get access token from Cornerstone OnDemand: %s", response.text
        )
        raise Exception("Failed to get access token from Cornerstone OnDemand")

    return response.json()["access_token"]


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobRequisitionsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    access_token = get_access_token(adapter, auth_parameters)

    base_url = BASE_URL_MAP[auth_parameters.environment].format(
        corpname=auth_parameters.corporation_name
    )

    headers = {"Authorization": f"Bearer {access_token}"}

    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    limit = params.pop("limit", 100)
    params["pageSize"] = limit if limit <= Page_size_limit else Page_size_limit
    params["page"] = 1

    jobs = []

    while True:
        response = requests.get(
            f"{base_url}/Recruiting/JobRequisitionDetails",
            headers=headers,
            params=params,
        )
        if response.status_code != 200:
            adapter.error(
                "Failed to read jobs from Cornerstone OnDemand: %s", response.text
            )
            break
        jobs.extend(response.json()["data"])

        if len(response.json()["data"]) == 0 or len(jobs) >= limit:
            break

        params["page"] += 1

    jobs = jobs[:limit]
    for job in jobs:
        yield job


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadApplicantsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    access_token = get_access_token(adapter, auth_parameters)

    base_url = BASE_URL_MAP[auth_parameters.environment].format(
        corpname=auth_parameters.corporation_name
    )

    headers = {"Authorization": f"Bearer {access_token}"}

    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)
    limit = params.pop("limit", 100)
    params["pageSize"] = limit if limit <= Page_size_limit else Page_size_limit
    params["page"] = 1

    profiles = []

    while True:
        response = requests.get(
            f"{base_url}/Recruiting/JobApplicant",
            headers=headers,
            params=params,
        )
        if response.status_code != 200:
            adapter.error(
                "Failed to read profiles from Cornerstone OnDemand: %s", response.text
            )
            break
        profiles.extend(response.json()["data"])

        if len(response.json()["data"]) == 0 or len(profiles) >= limit:
            break

        params["page"] += 1

    profiles = profiles[:limit]
    for profile in profiles:
        yield profile


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteCandidatesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    access_token = get_access_token(adapter, auth_parameters)
    base_url = BASE_URL_MAP[auth_parameters.environment].format(
        corpname=auth_parameters.corporation_name
    )
    headers = {"Authorization": f"Bearer {access_token}"}

    for item in items:
        resume_url = item.pop("resume", None)
        if resume_url:
            resume_file_response = requests.get(url=resume_url)
            resume = dict(
                file=resume_file_response.content,
                fileName="{}_{}_resume.pdf".format(item["FirstName"], item["LastName"]),
            )
        else:
            resume = None

        payload = dict(
            jobRequisitionId=parameters.jobRequisitionId,
            candidate=item,
            candidatePreferences=dict(
                futureOpportunityOptIn=parameters.futureOpportunityOptIn,
            ),
            source=asdict(parameters.source) if parameters.source else None,
            questions=(
                [asdict(question) for question in parameters.questions]
                if parameters.questions
                else None
            ),
            resume=resume,
            applicationPreferences=dict(
                sendEmail=parameters.sendEmail,
            ),
        )

        response = requests.post(
            f"{base_url}/x/candidate/v1/application",
            headers=headers,
            json=payload,
        )
        if response.status_code != 200:
            adapter.error(
                "Failed to write profile to Cornerstone OnDemand: %s", response.text
            )
            failed_profiles.append(item)

    return failed_profiles


JobsAisle = Aisle(
    name=Entity.job,
    schema=CornerstoneJobRequisition,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobRequisitionsParameters,
            update=ReadJobRequisitionsParameters,
            archive=ReadJobRequisitionsParameters,
        ),
        function=merge(create=read_jobs, update=read_jobs, archive=read_jobs),
    ),
)

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=CornerstoneApplicant,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadApplicantsParameters,
            update=ReadApplicantsParameters,
            archive=ReadApplicantsParameters,
        ),
        function=merge(
            create=read_profiles, update=read_profiles, archive=read_profiles
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(create=WriteCandidatesParameters),
        function=merge(create=write_profiles),
    ),
)
