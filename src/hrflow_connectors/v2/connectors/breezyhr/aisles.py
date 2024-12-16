import json
import typing as t
from datetime import datetime, timedelta
from enum import Enum
from io import BytesIO
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.breezyhr.schemas import (
    BreezyJobModel,
    BreezyProfileModel,
)
from hrflow_connectors.v2.core.common import Entity, Mode
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)

BREEZY_BASE_URL = "https://api.breezy.hr/v3"


class State(str, Enum):
    draft = "draft"
    archived = "archived"
    published = "published"
    closed = "closed"
    pending = "pending"


class Origin(str, Enum):
    sourced = "sourced"
    applied = "applied"


class AuthParameters(Struct):
    email: Annotated[
        str,
        Meta(
            description="email",
        ),
    ]
    password: Annotated[str, Meta(description="password")]
    company_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "ID of company to pull jobs from in Breezy HR database associated with"
                " the authenticated user"
            ),
        ),
    ] = None
    company_name: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "[⚠️ Requiered if company_id is not specified], the company associated"
                " with the authenticated user"
            ),
        ),
    ] = None


class ReadJobsParameters(Struct):
    state: Annotated[
        State,
        Meta(
            description=(
                "Specify an optional position state filter. e.g. draft, archived,"
                " published, closed, pending\nDefaults to published"
            ),
        ),
    ] = State.published


class WriteProfilesParameters(Struct):
    position_id: Annotated[
        str,
        Meta(
            description="Id of the position to create a new candidate for",
        ),
    ]
    origin: Annotated[
        Origin,
        Meta(
            description=(
                "will indicate in Breezy if the candidate should be marked as sourced"
                " or applied"
            ),
        ),
    ] = Origin.sourced
    stage_actions_enabled: Annotated[
        bool,
        Meta(
            description=(
                'When origin is "sourced", should stage actions be executed (defaults'
                ' to false). This is always true when origin is "applied"'
            ),
        ),
    ] = False


class UpdateProfilesParameters(Struct):
    position_id: Annotated[
        str,
        Meta(
            description="Id of the position to create a new candidate for",
        ),
    ]


class ReadProfilesParameters(Struct):
    position_id: Annotated[
        t.Optional[str],
        Meta(
            description="Id of the position to create a new candidate for",
        ),
    ] = None


def get_access_token(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
):
    sign_in_url = f"{BREEZY_BASE_URL}/signin"

    response = requests.post(
        sign_in_url,
        json={"email": auth_parameters.email, "password": auth_parameters.password},
    )

    if not response.ok:
        adapter.error(f"failed to retrieve access token, reason: {response.text}")
        raise Exception("failed to retrieve access token")

    return response.json().get("access_token")


def revoke_access_token(access_token):
    sign_out_url = f"{BREEZY_BASE_URL}/signout"

    headers = {"Authorization": f"{access_token}"}

    requests.get(sign_out_url, headers=headers)


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
        access_token = get_access_token(adapter, auth_parameters)
        headers = {"Authorization": access_token}

        company_id = auth_parameters.company_id
        if company_id is None:
            companies_url = f"{BREEZY_BASE_URL}/companies"

            companies_response = requests.get(companies_url, headers=headers)

            if not companies_response.ok:
                adapter.error(
                    f"Failed to retrieve company id, reason: {companies_response.text}"
                )
                raise Exception("Failed to retrieve company id")

            company_list = companies_response.json()
            for company in company_list:
                if company["name"] == auth_parameters.company_name:
                    company_id = company["_id"]
                    break
            if company_id is None:
                adapter.error(
                    "Failed to retrieve company id, reason: company does not match with"
                    " an id"
                )
                raise Exception("Failed to retrieve company id")

        url = f"{BREEZY_BASE_URL}/company/{company_id}/positions"
        params = {"state": parameters.state}

        response = requests.get(url, headers=headers, params=params)
        if not response.ok:
            adapter.error(f"Failed to read jobs, reason: {response.text}")
            raise Exception("Failed to read jobs")

        for job in response.json():
            if mode == Mode.create and not is_within_five_minutes(
                job["creation_date"], job["updated_date"]
            ):
                continue
            if mode == Mode.update and is_within_five_minutes(
                job["creation_date"], job["updated_date"]
            ):
                continue
            yield job

        revoke_access_token(access_token)

    return read_jobs


def send_profile(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
    access_token: str,
    company_id: str,
    candidate_id: str = "",
):
    base_url = (
        f"{BREEZY_BASE_URL}/company/{company_id}/"
        f"position/{parameters.position_id}/candidate"
    )

    payload = json.dumps(profiles)

    headers = {"Content-Type": "application/json", "Authorization": f"{access_token}"}

    if candidate_id == "":
        url = f"{base_url}s/"

        # If the candidate doesn't already exist we "POST" his profile
        response = requests.post(url, headers=headers, data=payload)

    else:
        # In case the candidate exists,
        #  we retrieve his id to update his profile with a "PUT" request

        url = f"{base_url}/{candidate_id}"

        url = (
            f"{BREEZY_BASE_URL}/company/{company_id}/"
            f"position/{parameters.position_id}/candidate/{candidate_id}"
        )

        adapter.info(f"Updating id = {candidate_id} profile")
        response = requests.put(url, headers=headers, data=payload)
    return response


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    access_token = get_access_token(adapter, auth_parameters)
    headers = {"Authorization": access_token}

    company_id = auth_parameters.company_id

    if company_id is None:
        companies_url = f"{BREEZY_BASE_URL}/companies"

        companies_response = requests.get(companies_url, headers=headers)

        if not companies_response.ok:
            adapter.error(
                f"Failed to retrieve company id, reason: {companies_response.text}"
            )
            raise Exception("Failed to retrieve company id")

        company_list = companies_response.json()
        for company in company_list:
            if company["name"] == auth_parameters.company_name:
                company_id = company["_id"]
                break
        if company_id is None:
            adapter.error(
                "Failed to retrieve company id, reason: company does not match with"
                " an id"
            )
            raise Exception("Failed to retrieve company id")

    post_candidate_url = (
        f"{BREEZY_BASE_URL}/company/{company_id}/position/"
        f"{parameters.position_id}/candidates"
    )

    for profile in items:
        resume_url = profile.pop("resume", None)
        profile.update({"origin": parameters.origin})
        if parameters.stage_actions_enabled:
            params = {"stage_actions_enabled": parameters.stage_actions_enabled}
        else:
            params = {}

        post_candidate_response = requests.post(
            post_candidate_url, headers=headers, json=profile, params=params
        )
        if not post_candidate_response.ok:
            adapter.error(
                f"Couldn't create candidate, reason: {post_candidate_response.text}"
            )
            failed_profiles.append(profile)
            continue

        if resume_url:
            candidate_id = post_candidate_response.json()["_id"]
            resume_file_response = requests.get(url=resume_url)
            resume_file_obj = BytesIO(resume_file_response.content)
            files = {
                "file": (
                    "resume.pdf",
                    resume_file_obj,
                    "application/pdf",
                )
            }
            post_resume_url = (
                f"{BREEZY_BASE_URL}/company/{company_id}/position/"
                f"{parameters.position_id}/candidate/{candidate_id}/resume"
            )
            post_resume_response = requests.post(
                post_resume_url, headers=headers, files=files
            )
            if not post_resume_response.ok:
                adapter.error(
                    "Failed to attach resume to candidate, reason:"
                    f" {post_resume_response.text}"
                )

    revoke_access_token(access_token)
    return failed_profiles


def update_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    access_token = get_access_token(adapter, auth_parameters)
    headers = {"Authorization": access_token}

    company_id = auth_parameters.company_id
    if company_id is None:
        companies_url = f"{BREEZY_BASE_URL}/companies"

        companies_response = requests.get(companies_url, headers=headers)

        if not companies_response.ok:
            adapter.error(
                f"Failed to retrieve company id, reason: {companies_response.text}"
            )
            raise Exception("Failed to retrieve company id")

        company_list = companies_response.json()
        for company in company_list:
            if company["name"] == auth_parameters.company_name:
                company_id = company["_id"]
                break
        if company_id is None:
            adapter.error(
                "Failed to retrieve company id, reason: company does not match with"
                " an id"
            )
            raise Exception("Failed to retrieve company id")

    base_url = (
        f"{BREEZY_BASE_URL}/company/{company_id}/position/"
        f"{parameters.position_id}/candidate"
    )

    for profile in items:
        candidate_id = profile.pop("id")
        candidate_work_history = profile.pop("work_history", [])
        candidate_education = profile.pop("education", [])
        put_candidate_url = f"{base_url}/{candidate_id}"
        put_candidate_response = requests.put(
            put_candidate_url, headers=headers, json=profile
        )
        if not put_candidate_response.ok:
            adapter.error(
                f"Couldn't create candidate, reason: {put_candidate_response.text}"
            )
            failed_profiles.append(profile)
            continue

        current_educations = [
            {education["school_name"], education["field_of_study"]}
            for education in put_candidate_response.json().get("education", [])
        ]
        current_work_history = [
            {work_history["company_name"], work_history["title"]}
            for work_history in put_candidate_response.json().get("work_history", [])
        ]

        for education in candidate_education:
            if {
                education["school_name"],
                education["field_of_study"],
            } in current_educations:
                continue
            education_url = f"{put_candidate_url}/education"
            education_response = requests.put(
                education_url, headers=headers, json=education
            )
            if not education_response.ok:
                adapter.error(
                    f"Couldn't update education, for candidate {candidate_id}, reason:"
                    f" {education_response.text}"
                )
        for work_history in candidate_work_history:
            if {
                work_history["company_name"],
                work_history["title"],
            } in current_work_history:
                continue
            work_history_url = f"{put_candidate_url}/work-history"
            work_history_response = requests.put(
                work_history_url, headers=headers, json=work_history
            )
            if not work_history_response.ok:
                adapter.error(
                    f"Couldn't update experience, for candidate {candidate_id}, reason:"
                    f" {work_history_response.text}"
                )

    revoke_access_token(access_token)
    return failed_profiles


def generic_profiles_read(
    mode: Mode,
):
    def read_profiles(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadProfilesParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[t.Dict]:
        # TODO: add incremental read_mode using page_size, and page as read_from
        access_token = get_access_token(adapter, auth_parameters)
        headers = {"Authorization": access_token}

        company_id = auth_parameters.company_id
        if company_id is None:
            companies_url = f"{BREEZY_BASE_URL}/companies"

            companies_response = requests.get(companies_url, headers=headers)

            if not companies_response.ok:
                adapter.error(
                    f"Failed to retrieve company id, reason: {companies_response.text}"
                )
                raise Exception("Failed to retrieve company id")

            company_list = companies_response.json()
            for company in company_list:
                if company["name"] == auth_parameters.company_name:
                    company_id = company["_id"]
                    break
            if company_id is None:
                adapter.error(
                    "Failed to retrieve company id, reason: company does not match with"
                    " an id"
                )
                raise Exception("Failed to retrieve company id")
        if not parameters.position_id:
            # retrieve all postion ids for all published positions
            positions_url = (
                f"{BREEZY_BASE_URL}/company/{company_id}/positions?state=published"
            )

            positions_response = requests.get(positions_url, headers=headers)
            if len(positions_response.json()) == 0:
                adapter.info("No published position found")
            for position in positions_response.json():
                position_id = position["_id"]
                candidates_url = (
                    f"{BREEZY_BASE_URL}/company/{company_id}/position/"
                    f"{position_id}/candidates?sort=updated"
                )
                candidates_response = requests.get(candidates_url, headers=headers)
                if not candidates_response.ok:
                    adapter.error(
                        "Failed to retrieve candidates, reason:"
                        f" {candidates_response.text}"
                    )
                    raise Exception("Failed to retrieve candidates")
                candidates = candidates_response.json()
                for candidate in candidates:
                    if mode == Mode.create and not is_within_five_minutes(
                        candidate["creation_date"], candidate["updated_date"]
                    ):
                        continue
                    if mode == Mode.update and is_within_five_minutes(
                        candidate["creation_date"], candidate["updated_date"]
                    ):
                        continue
                    candidate_id = candidate["_id"]
                    full_candidate_url = (
                        f"{BREEZY_BASE_URL}/company/{company_id}/position/"
                        f"{position_id}/candidate/{candidate_id}"
                    )
                    full_candidate_response = requests.get(
                        full_candidate_url, headers=headers
                    )
                    if not full_candidate_response.ok:
                        adapter.error(
                            "Failed to retrieve candidate, reason:"
                            f" {full_candidate_response.text}"
                        )
                        continue

                    full_candidate = full_candidate_response.json()

                    resume_url = full_candidate.get("resume", {}).get("url")
                    if not resume_url:
                        full_candidate["resume"] = None
                    if resume_url:
                        resume_response = requests.get(resume_url, headers=headers)
                        if not resume_response.ok:
                            adapter.error(
                                "Failed to retrieve resume, reason:"
                                f" {resume_response.text}"
                            )
                            full_candidate["resume"] = None
                        else:
                            full_candidate["resume"] = dict(raw=resume_response.content)

                    yield full_candidate
        else:
            candidates_url = (
                f"{BREEZY_BASE_URL}/company/{company_id}/position/"
                f"{parameters.position_id}/candidates?sort=updated"
            )

            candidates_response = requests.get(candidates_url, headers=headers)
            if not candidates_response.ok:
                adapter.error(
                    f"Failed to retrieve candidates, reason: {candidates_response.text}"
                )
                raise Exception("Failed to retrieve candidates")
            candidates = candidates_response.json()
            for candidate in candidates:
                if mode == Mode.create and not is_within_five_minutes(
                    candidate["creation_date"], candidate["updated_date"]
                ):
                    continue
                if mode == Mode.update and is_within_five_minutes(
                    candidate["creation_date"], candidate["updated_date"]
                ):
                    continue
                candidate_id = candidate["_id"]
                full_candidate_url = (
                    f"{BREEZY_BASE_URL}/company/{company_id}/position/"
                    f"{parameters.position_id}/candidate/{candidate_id}"
                )
                full_candidate_response = requests.get(
                    full_candidate_url, headers=headers
                )
                if not full_candidate_response.ok:
                    adapter.error(
                        "Failed to retrieve candidate, reason:"
                        f" {full_candidate_response.text}"
                    )
                    continue

                full_candidate = full_candidate_response.json()

                resume_url = full_candidate.get("resume", {}).get("url")
                if not resume_url:
                    full_candidate["resume"] = None
                if resume_url:
                    resume_response = requests.get(resume_url, headers=headers)
                    if not resume_response.ok:
                        adapter.error(
                            f"Failed to retrieve resume, reason: {resume_response.text}"
                        )
                        full_candidate["resume"] = None
                    else:
                        full_candidate["resume"] = dict(raw=resume_response.content)

                yield full_candidate
        revoke_access_token(access_token)

    return read_profiles


# To account for the time differnece between the start of object creation on the platform
# and its completion
def is_within_five_minutes(date_str1: str, date_str2: str) -> bool:
    date1 = datetime.fromisoformat(date_str1.replace("Z", "+00:00"))
    date2 = datetime.fromisoformat(date_str2.replace("Z", "+00:00"))

    difference = abs(date1 - date2)

    return difference <= timedelta(minutes=5)


JobsAisle = Aisle(
    name=Entity.job,
    schema=BreezyJobModel,
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

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=BreezyProfileModel,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=generic_profiles_read(Mode.create),
            update=generic_profiles_read(Mode.update),
            archive=generic_profiles_read(Mode.archive),
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters, update=UpdateProfilesParameters
        ),
        function=merge(create=write_profiles, update=update_profiles),
    ),
)
