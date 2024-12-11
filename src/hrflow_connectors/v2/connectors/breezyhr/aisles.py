import json
import typing as t
from enum import Enum
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


class ReadJobsParameters(Struct):
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
    company_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "ID of company to pull jobs from in Breezy HR database associated with"
                " the authenticated user \n [⚠️ Requiered if company_name is not"
                " specified]"
            ),
        ),
    ] = None
    company_name: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "the company associated with the authenticated user \n [⚠️ Requiered if"
                " company_id is not specified]"
            ),
        ),
    ] = None

    origin: Annotated[
        Origin,
        Meta(
            description=(
                "will indicate in Breezy if the candidate should be marked as sourced"
                " or applied"
            ),
        ),
    ] = Origin.sourced


class UpdateProfilesParameters(Struct):
    position_id: Annotated[
        str,
        Meta(
            description="Id of the position to create a new candidate for",
        ),
    ]
    company_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "ID of company to pull jobs from in Breezy HR database associated with"
                " the authenticated user \n [⚠️ Requiered if company_name is not"
                " specified]"
            ),
        ),
    ] = None
    company_name: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "the company associated with the authenticated user \n [⚠️ Requiered if"
                " company_id is not specified]"
            ),
        ),
    ] = None


class ReadProfilesParameters(Struct):
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


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    access_token = get_access_token(adapter, auth_parameters)
    headers = {"Authorization": access_token}

    company_id = parameters.company_id
    if company_id is None:
        url = f"{BREEZY_BASE_URL}/companies"

        response = requests.get(url, headers=headers)

        if not response.ok:
            adapter.error(f"Failed to retrieve company id, reason: {response.text}")
            raise Exception("Failed to retrieve company id")

        company_list = response.json()
        for company in company_list:
            if company["name"] == parameters.company_name:
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
        yield job

    revoke_access_token(access_token)


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

    company_id = parameters.company_id
    if company_id is None:
        url = f"{BREEZY_BASE_URL}/companies"

        response = requests.get(url, headers=headers)

        if not response.ok:
            adapter.error(f"Failed to retrieve company id, reason: {response.text}")
            raise Exception("Failed to retrieve company id")

        company_list = response.json()
        for company in company_list:
            if company["name"] == parameters.company_name:
                company_id = company["_id"]
                break
        if company_id is None:
            adapter.error(
                "Failed to retrieve company id, reason: company does not match with"
                " an id"
            )
            raise Exception("Failed to retrieve company id")

    url = f"{BREEZY_BASE_URL}/company/{company_id}/positions/{parameters.position_id}/candidates"
    for profile in items:
        profile.update({"origin": parameters.origin})

        response = requests.post(url, headers=headers, json=profile)
        if not response.ok:
            adapter.error(f"Couldn't create candidate, reason: {response.text}")
            failed_profiles.append(profile)
            continue

        # TODO: attach resume to candidate
        resume_url = (
            f"{BREEZY_BASE_URL}/company/{company_id}/position/"
            f"{parameters.position_id}/candidate/{response.json()['_id']}/resume"
        )
        payload = {
            "file": profile.get("resume"),
        }
        resume_response = requests.post(resume_url, headers=headers, json=payload)
        if not resume_response.ok:
            adapter.error(
                f"Failed to attach resume to candidate, reason: {resume_response.text}"
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

    company_id = parameters.company_id
    if company_id is None:
        url = f"{BREEZY_BASE_URL}/companies"

        response = requests.get(url, headers=headers)

        if not response.ok:
            adapter.error(f"Failed to retrieve company id, reason: {response.text}")
            raise Exception("Failed to retrieve company id")

        company_list = response.json()
        for company in company_list:
            if company["name"] == parameters.company_name:
                company_id = company["_id"]
                break
        if company_id is None:
            adapter.error(
                "Failed to retrieve company id, reason: company does not match with"
                " an id"
            )
            raise Exception("Failed to retrieve company id")

    base_url = f"{BREEZY_BASE_URL}/company/{company_id}/positions/{parameters.position_id}/candidate"
    for profile in items:
        profile_id = profile.pop("id")
        url = f"{base_url}/{profile_id}"
        response = requests.put(url, headers=headers, json=profile)
        if not response.ok:
            adapter.error(f"Couldn't create candidate, reason: {response.text}")
            failed_profiles.append(profile)
            continue

    revoke_access_token(access_token)
    return failed_profiles


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

    company_id = parameters.company_id
    if company_id is None:
        url = f"{BREEZY_BASE_URL}/companies"

        response = requests.get(url, headers=headers)

        if not response.ok:
            adapter.error(f"Failed to retrieve company id, reason: {response.text}")
            raise Exception("Failed to retrieve company id")

        company_list = response.json()
        for company in company_list:
            if company["name"] == parameters.company_name:
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

        response = requests.get(positions_url, headers=headers)
        if len(response.json()) == 0:
            adapter.info("No published position found")
        for position in response.json():
            position_id = position["_id"]
            url = (
                f"{BREEZY_BASE_URL}/company/{company_id}/position/"
                f"{position_id}/candidates?sort=updated"
            )
            candidates = requests.get(url, headers=headers).json()
            for candidate in candidates:
                candidate_id = candidate["_id"]
                url = (
                    f"{BREEZY_BASE_URL}/company/{company_id}/position/"
                    f"{position_id}/candidate/{candidate_id}"
                )
                yield requests.get(url, headers=headers).json()
    else:
        url = (
            f"{BREEZY_BASE_URL}/company/{company_id}/position/"
            f"{parameters.position_id}/candidates?sort=updated"
        )

        response = requests.get(url, headers=headers)
        for candidate in response.json():
            candidate_id = candidate["_id"]
            url = (
                f"{BREEZY_BASE_URL}/company/{company_id}/position/"
                f"{parameters.position_id}/candidate/{candidate_id}"
            )
            yield requests.get(url, headers=headers).json()

    revoke_access_token(access_token)


JobsAisle = Aisle(
    name=Entity.job,
    schema=BreezyJobModel,
    read=ReadOperation(
        criterias=Criterias(create=ReadJobsParameters),
        function=merge(create=read_jobs),
    ),
)

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=BreezyProfileModel,
    read=ReadOperation(
        criterias=Criterias(create=ReadProfilesParameters),
        function=merge(create=read_profiles),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters, update=UpdateProfilesParameters
        ),
        function=merge(create=write_profiles, update=update_profiles),
    ),
)
