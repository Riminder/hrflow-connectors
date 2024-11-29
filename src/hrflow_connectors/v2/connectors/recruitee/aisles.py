import typing as t
from datetime import datetime, timedelta
from enum import Enum
from io import BytesIO
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec.structs import asdict
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.recruitee.schemas import (
    RecruiteeJob,
    RecruiteeProfile,
)
from hrflow_connectors.v2.core.common import Entity, Mode
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)


class Sort(str, Enum):
    BY_DATE = "by_date"
    BY_LAST_MESSAGE = "by_last_message"


class RecruiteeEndpoint(str, Enum):
    DEVELOPMENT_ENDPOINT = "https://api.rc.recruitee.dev/c"
    STAGING_ENDPOINT = "https://api.s.recruitee.com/c"
    PRODUCTION_ENDPOINT = "https://api.recruitee.com/c"


class AuthParameters(Struct):
    company_id: Annotated[
        str,
        Meta(
            description="Company ID. A company subdomain can also be used.",
        ),
    ]
    api_token: Annotated[
        str,
        Meta(
            description=(
                "Personal API Token allowing access to the Recruitee API from external"
                " services."
            ),
        ),
    ]
    recruitee_endpoint: Annotated[
        RecruiteeEndpoint,
        Meta(
            description="Specifies which endpoint to be used, satging or production.",
        ),
    ] = RecruiteeEndpoint.PRODUCTION_ENDPOINT


class ReadProfilesParameters(Struct):
    limit: Annotated[
        t.Optional[int],
        Meta(
            description="Specifies the number of candidates to retrieve",
        ),
    ] = None
    offset: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Skip number of candidates from the begining, used for ‘load more’,"
                " offset for next page should be current offset + limit"
            ),
        ),
    ] = None
    created_after: Annotated[
        t.Optional[str],
        Meta(
            description="Show only candidates created after given date",
        ),
    ] = None
    disqualified: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                """Show only disqualified candidates who are disqualified in at least"""
                """ one job (should be string ‘true’ or ‘1’)."""
            ),
        ),
    ] = None
    qualified: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "Show only disqualified candidates who are qualified in at least one"
                " job (should be string ‘true’ or ‘1’)."
            ),
        ),
    ] = None
    ids: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "List of IDs separated by comma, example: 234221,4211412,535432"
            ),
        ),
    ] = None
    offer_id: Annotated[
        t.Optional[str],
        Meta(
            description="Filter by offer",
        ),
    ] = None
    query: Annotated[
        t.Optional[str],
        Meta(
            description="Search query for candidate’s name or offer",
        ),
    ] = None
    sort: Annotated[
        t.Optional[Sort],
        Meta(
            description="Sorting options: by_date, by_last_message",
        ),
    ] = None
    with_messages: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "Show only candidates with messages (should be string ‘true’ or ‘1’)"
            ),
        ),
    ] = None
    with_my_messages: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "Show only candidates with messages that current admin sent (should be"
                " string ‘true’ or ‘1’"
            ),
        ),
    ] = None


class WriteProfilesParameters(Struct):
    offers: Annotated[
        t.Optional[t.List[int]],
        Meta(
            description=(
                "Offers to which the candidate will be assigned with default stage."
            ),
        ),
    ] = None


class UpdateProfilesParameters(Struct):
    pass


class ArchiveProfilesParameters(Struct):
    pass


class Kind(str, Enum):
    JOB = "job"
    TALENT_POOL = "talent_pool"


class Scope(str, Enum):
    ARCHIVED = "archived"
    ACTIVE = "active"
    NOT_ARCHIVED = "not_archived"


class View_mode(str, Enum):
    DEFAULT = "default"
    BRIEF = "brief"


class ReadJobsParameters(Struct):
    kind: Annotated[
        t.Optional[Kind],
        Meta(
            description=(
                "If no kind is given, returns all job offers, if kind is job then lists"
                " only jobs, if scope is talent_pool, lists only talent pools"
            ),
        ),
    ] = Kind.JOB
    scope: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "If no scope is given list all job offers. archived returns only"
                " archived job offers, active returns published, internal and closed"
                " job offers, not_archived returns all but archived jobs"
            ),
        ),
    ] = Scope.ACTIVE
    view_mode: Annotated[
        View_mode,
        Meta(
            description=(
                "default (default mode, includes most of offer details); brief (only"
                " offer’s id, title, status and kind)"
            ),
        ),
    ] = View_mode.DEFAULT


def generic_candidates_read(
    mode: Mode,
):
    def read_candidates(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadProfilesParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[t.Dict]:
        RECRUITEE_ENDPOINT = auth_parameters.recruitee_endpoint
        params = asdict(parameters)

        response = requests.get(
            "{}/{}/candidates".format(RECRUITEE_ENDPOINT, auth_parameters.company_id),
            headers={
                "Authorization": "Bearer {}".format(auth_parameters.api_token),
            },
            params=params,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull candidates list from Recruitee params={}"
                " status_code={} response={}".format(
                    params, response.status_code, response.text
                )
            )
        all_candidates = response.json()["candidates"]
        for candidate in all_candidates:
            if mode == Mode.create and not is_within_five_minutes(
                candidate["created_at"], candidate["updated_at"]
            ):
                continue
            if mode == Mode.update and is_within_five_minutes(
                candidate["created_at"], candidate["updated_at"]
            ):
                continue
            full_candidate_response = requests.get(
                "{}/{}/candidates/{}".format(
                    RECRUITEE_ENDPOINT, auth_parameters.company_id, candidate["id"]
                ),
                headers={
                    "Authorization": "Bearer {}".format(auth_parameters.api_token),
                },
            )
            if full_candidate_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to pull candidate details from Recruitee candidate_id={}"
                    " status_code={} response={}".format(
                        candidate["id"],
                        full_candidate_response.status_code,
                        full_candidate_response.text,
                    )
                )

            candidate = full_candidate_response.json()["candidate"]
            resume_url = candidate.get("cv_original_url")
            if resume_url:
                response = requests.get(url=resume_url)
                file = response.content
                profile_file = BytesIO(file)
                candidate["cv_file"] = profile_file
            else:
                candidate["cv_file"] = None

            yield candidate

    return read_candidates


def write_candidates(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[dict],
) -> t.List[t.Dict]:
    RECRUITEE_ENDPOINT = auth_parameters.recruitee_endpoint
    failed_profiles = []

    for profile in items:
        payload = {"candidate": profile, "offers": parameters.offers}

        response = requests.post(
            "{}/{}/candidates".format(RECRUITEE_ENDPOINT, auth_parameters.company_id),
            headers={
                "Accept": "application/json",
                "Content-Type": "application/json",
                "Authorization": "Bearer {}".format(auth_parameters.api_token),
            },
            json=payload,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to add candidate to Recruitee status_code={} response={}"
                .format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


def update_candidates(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateProfilesParameters,
    items: t.Iterable[dict],
) -> t.List[t.Dict]:
    RECRUITEE_ENDPOINT = auth_parameters.recruitee_endpoint
    failed_profiles = []

    for profile in items:
        candidate_id = profile.pop("id")
        payload = {"candidate": profile}

        response = requests.patch(
            "{}/{}/candidates/{}".format(
                RECRUITEE_ENDPOINT, auth_parameters.company_id, candidate_id
            ),
            headers={
                "Content-Type": "application/json",
                "Authorization": "Bearer {}".format(auth_parameters.api_token),
            },
            json=payload,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to update candidate with id={} in Recruitee status_code={}"
                " response={}".format(
                    candidate_id,
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)

    return failed_profiles


def archive_candidates(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ArchiveProfilesParameters,
    items: t.Iterable[dict],
) -> t.List[t.Dict]:
    RECRUITEE_ENDPOINT = auth_parameters.recruitee_endpoint
    failed_profiles = []

    for profile in items:
        response = requests.delete(
            "{}/{}/candidates/{}".format(
                RECRUITEE_ENDPOINT, auth_parameters.company_id, profile["id"]
            ),
            headers={
                "Authorization": "Bearer {}".format(auth_parameters.api_token),
            },
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to archive candidate with id={} in Recruitee in Recruitee"
                " status_code={} response={}".format(
                    profile["id"],
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)

    return failed_profiles


def generic_jobs_read(
    mode: Mode,
):
    def read_jobs(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadJobsParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[t.Dict]:
        RECRUITEE_ENDPOINT = auth_parameters.recruitee_endpoint
        params = asdict(parameters)

        response = requests.get(
            "{}/{}/offers".format(RECRUITEE_ENDPOINT, auth_parameters.company_id),
            headers={"Authorization": "Bearer {}".format(auth_parameters.api_token)},
            params=params,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Recruitee params={}"
                " status_code={} response={}".format(
                    params, response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from Recruitee")
        jobs = response.json()["offers"]
        for job in jobs:
            if mode == Mode.create and not is_within_five_minutes(
                job["created_at"], job["updated_at"]
            ):
                continue
            if mode == Mode.update and is_within_five_minutes(
                job["created_at"], job["updated_at"]
            ):
                continue
            full_job_response = requests.get(
                "{}/{}/offers/{}".format(
                    RECRUITEE_ENDPOINT, auth_parameters.company_id, job["id"]
                ),
                headers={
                    "Authorization": "Bearer {}".format(auth_parameters.api_token)
                },
            )
            if full_job_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to pull jobs details from Recruitee job_id={}"
                    " status_code={} response={}".format(
                        job["id"],
                        full_job_response.status_code,
                        full_job_response.text,
                    )
                )
                raise Exception("Failed to pull jobs from Recruitee")
            yield full_job_response.json()["offer"]

    return read_jobs


# To account for the time differnece between the start of job creation on the platform
# and its completion
def is_within_five_minutes(date_str1: str, date_str2: str) -> bool:
    date1 = datetime.fromisoformat(date_str1.replace("Z", "+00:00"))
    date2 = datetime.fromisoformat(date_str2.replace("Z", "+00:00"))

    difference = abs(date1 - date2)

    return difference <= timedelta(minutes=5)


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=RecruiteeProfile,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=generic_candidates_read(Mode.create),
            update=generic_candidates_read(Mode.update),
            archive=generic_candidates_read(Mode.archive),
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=UpdateProfilesParameters,
            archive=ArchiveProfilesParameters,
        ),
        function=merge(
            create=write_candidates,
            update=update_candidates,
            archive=archive_candidates,
        ),
    ),
)


JobsAisle = Aisle(
    name=Entity.job,
    schema=RecruiteeJob,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=generic_jobs_read(Mode.create),
            update=generic_jobs_read(Mode.update),
            archive=generic_jobs_read(Mode.archive),
        ),
    ),
)
