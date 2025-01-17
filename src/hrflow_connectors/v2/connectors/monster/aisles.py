import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.monster.schemas import MonsterProfile
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import Aisle, Criterias, ReadOperation, merge


class SearchType(str, Enum):
    JobDetail = "JobDetail"
    Semantic = "Semantic"


class Importance(str, Enum):
    Required = "Required"
    NiceToHave = "NiceToHave"


class Skill(Struct):
    name: str
    importance: Annotated[
        Importance,
        Meta(description="Possible values: Required, NiceToHave(default)"),
    ] = Importance.NiceToHave


class RadiusUnit(str, Enum):
    # For Miles:
    Miles = "Miles"
    Mi = "Mi"
    M = "M"
    # For Kilometers:
    Kilometers = "Kilometers"
    Kilometres = "Kilometres"  # international standard
    Km = "Km"
    K = "K"


class Location(Struct):
    city: Annotated[
        str,
        Meta(
            description=(
                "City name of the location. E.g. 'New York City', 'Boston', etc."
            )
        ),
    ]
    state: Annotated[
        str,
        Meta(description="The state abbrev of the location. E.g. 'NY', 'CA', etc."),
    ]
    postalCode: Annotated[
        str,
        Meta(description="Postal code of the location. E.g. '0186'."),
    ]
    radius: Annotated[
        int,
        Meta(
            description=(
                "Radius of this location search (considering the radius unit), default"
                " 25 miles."
            )
        ),
    ] = 25
    radiusUnit: Annotated[
        RadiusUnit,
        Meta(
            description=(
                "Radius Unit of this location search. Allowed values (case"
                " insensitive): For Miles (default): 'Miles', 'Mi', 'M' For Kilometers:"
                " 'Kilometers', 'Kilometres' (international standard), 'Km', 'K'"
            )
        ),
    ] = RadiusUnit.Miles
    locationExpression: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "This field is cannot be used in combination with the other location"
                " fields. The encoded location expression may contain the city, state,"
                " postal code or other location description (ex Los Angeles County,"
                " CA). Multiple expressions are comma separated. Radius may be included"
                " in the expression or radius may be used."
            )
        ),
    ] = None


class Degree(Struct):
    degreeName: str
    importance: Annotated[
        Importance,
        Meta(description="Possible values: Required, NiceToHave(default)"),
    ] = Importance.NiceToHave


class SemanticSearchParameters(Struct):
    # The basic parameters
    jobTitles: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="A comma separated list of Job Titles to search.",
        ),
    ] = None
    skills: Annotated[
        t.Optional[t.List[Skill]],
        Meta(
            description=(
                "List of all skills to be searched with an optional importance."
            ),
        ),
    ] = None
    locations: Annotated[
        t.Optional[t.List[Location]],
        Meta(
            description=(
                "List of all locations to be included in search, composed of the city,"
                " state, postal code, and radius. It's also possible to provide a"
                " location expression."
            ),
        ),
    ] = None
    # Additional Resume-Based Search Parameters
    yearsOfExperience: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Expression to describe the number of years’ experience for the search."
                " Calculated from date ranges in the work history section of the"
                " resume. Accepted format:\n·Single number of years (Ex.: '1');"
                " or\n·Range of years (Ex.: '1-5' inclusive); or\n·Greater than"
                " expression (Ex.: '>5'); or\n·Greater than expression (Ex.: '5+'"
                " encoded as '5%2b'); or\n·Less than expression (Ex.: '<3', '<8')."
            ),
        ),
    ] = None
    schools: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="A comma separated list of schools.",
        ),
    ] = None
    degrees: Annotated[
        t.Optional[t.List[Degree]],
        Meta(
            description="A comma separated list of degrees with optional importance.",
        ),
    ] = None
    companies: Annotated[
        t.Optional[t.List[str]],
        Meta(
            description="A comma separated list of companies.",
        ),
    ] = None
    jobTenure: Annotated[
        t.Optional[float],
        Meta(
            description=(
                "Average tenure for all jobs on a resume, specified in fractions of a"
                " year. E.g. 0.5 (means 6 months). 1 (means 1 year)."
            ),
        ),
    ] = None
    # Monster Activity Indicators
    lastActiveMaximumAge: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Maximum time since the seeker was active on Monster (in minutes). Does"
                " not apply to private resume databases."
            ),
        ),
    ] = None
    resumeUpdatedMaximumAge: Annotated[
        t.Optional[int],
        Meta(
            description="Maximum time since resume was updated (in minutes).",
        ),
    ] = None
    resumeUpdatedMinimumAge: Annotated[
        t.Optional[int],
        Meta(
            description="Minimum time since resume was updated (in minutes).",
        ),
    ] = None


class JobDetailSearchParameters(Struct):
    jobTitle: Annotated[
        t.Optional[str],
        Meta(
            description="Title of the job. Multiple titles may be separated commas.",
        ),
    ] = None
    jobDescription: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Full text of the Job Description. The text may contain HTML. Special"
                " characters need to be escaped to comply with standard JSON encoding."
            ),
        ),
    ] = None
    locations: Annotated[
        t.Optional[t.List[Location]],
        Meta(
            description=(
                "List of all locations to be included in search, composed of the city,"
                " state, postal code, and radius. It's also possible to provide a"
                " location expression."
            ),
        ),
    ] = None
    jobId: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "An optional method for identifying a job based on the Monster posting"
                " ID. The string is a formatted GUID."
            ),
        ),
    ] = None
    jobDescriptionBase64: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Optionally, the job description can be provided as a Base64 encoded"
                " string."
            ),
        ),
    ] = None


class AuthParameters(Struct):
    client_id: Annotated[
        str,
        Meta(
            description="Client ID that your Monster representative provided you with.",
        ),
    ]
    client_secret: Annotated[
        str,
        Meta(
            description=(
                "Client Secret that your Monster representative provided you with."
            ),
        ),
    ]
    easy_apply_key: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Easy Apply API key that your Monster representative provided you with."
            ),
        ),
    ] = None


class ReadProfilesParameters(Struct):
    searchType: Annotated[
        SearchType,
        Meta(
            description=(
                "Defines if the user is providing parameters for the semantic search"
                " engine or matching with a job description/id. Possible"
                " values:\n·'JobDetail'. Automated matching using Monster AI to"
                " construct a search based on a job title, job description and"
                " location.\n·'Semantic'. Full function semantic search and hybrid"
                " semantic/Boolean search."
            ),
        ),
    ]
    semantic: Annotated[
        t.Optional[SemanticSearchParameters],
        Meta(
            description="Semantic search parameters.",
        ),
    ] = None
    JobDetail: Annotated[
        t.Optional[JobDetailSearchParameters],
        Meta(
            description="JobDetail search parameters.",
        ),
    ] = None
    candidateName: Annotated[
        t.Optional[str],
        Meta(
            description="Name of a single candidate to look for.",
        ),
    ] = None


def authenticate(
    adapter: LoggerAdapter,
    client_id: str,
    client_secret: str,
) -> str:
    response = requests.post(
        "https://sso.monster.com/core/connect/token",
        headers={"content-type": "application/x-www-form-urlencoded"},
        data={
            "client_id": client_id,
            "client_secret": client_secret,
            "scope": "GatewayAccess",
            "grant_type": "client_credentials",
        },
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to authenticate to Monster  status_code={} response={}".format(
                response.status_code,
                response.text,
            )
        )
    return response.json()["access_token"]


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    access_token = authenticate(
        adapter, auth_parameters.client_id, auth_parameters.client_secret
    )

    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    endpoint_url = "https://api.jobs.com/v2/candidates/queries?page=1"
    profiles = []
    while True:
        response = requests.post(
            endpoint_url,
            headers={
                "content-type": "application/json",
                "Authorization": "bearer {}".format(access_token),
            },
            json=params,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to read profiles from Monster  status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )

        profiles.extend(response.json()["results"])

        if "link" in response.headers:
            links = response.headers["link"]
            for link in links.split(","):
                url, rel = link.split(";")
                url = url.strip().strip("<>")
                rel = rel.strip()
                if rel == 'rel="next"':
                    next_link = url
                    break
            else:
                next_link = None
        else:
            next_link = None

        if next_link is None:
            break
        endpoint_url = next_link

    for profile in profiles:
        yield profile


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=MonsterProfile,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read_profiles, update=read_profiles, archive=read_profiles
        ),
    ),
)
