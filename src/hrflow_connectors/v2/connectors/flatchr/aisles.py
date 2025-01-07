import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.flatchr.schemas import (
    FlatchrProfile,
    FlatchrVacancy,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    Endpoint,
    Endpoints,
    ReadOperation,
    WriteOperation,
    merge,
)

FLATCHR_CAREERS_BASE_URL = "https://careers.flatchr.io"


class FlatchrBaseURL(str, Enum):
    PRODUCTION = "https://api.flatchr.io/"
    TESTING = "https://api.demo.flatchr.io"


class AuthParameters(Struct):
    api_key: Annotated[
        str,
        Meta(
            description="The API key to authenticate with the Flatchr API",
        ),
    ]
    company_id: Annotated[
        str,
        Meta(
            description="The ID of the company to authenticate with",
        ),
    ]
    env_base_url: Annotated[
        FlatchrBaseURL,
        Meta(
            description="The base URL of the Flatchr API",
        ),
    ] = FlatchrBaseURL.PRODUCTION


class ReadProfilesParameters(Struct, omit_defaults=True):
    firstname: Annotated[
        t.Optional[str],
        Meta(
            description="The firstname of the candidate to search for",
        ),
    ] = None
    lastname: Annotated[
        t.Optional[str],
        Meta(
            description="The lastname of the candidate to search for",
        ),
    ] = None
    email: Annotated[
        t.Optional[str],
        Meta(
            description="The email of the candidate to search for",
        ),
    ] = None
    hired: Annotated[
        t.Optional[bool],
        Meta(
            description="Whether the candidate has been hired or not",
        ),
    ] = None
    column: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The column in which the candidate is located, Ex: 'Entretien RH'"
            ),
        ),
    ] = None
    start: Annotated[
        t.Optional[str],
        Meta(
            description="The start date in MM/DD/YY of the search",
        ),
    ] = None
    end: Annotated[
        t.Optional[str],
        Meta(
            description="The end date in MM/DD/YY of the search",
        ),
    ] = None
    vacancy: Annotated[
        t.Optional[str],
        Meta(description="id of the offer in which the candidate is involved"),
    ] = None


class WriteProfilesParameters(Struct):
    vacancy_slug: Annotated[
        str,
        Meta(
            description="The slug of the offer to assign the candidate to",
        ),
    ]


class UpdateProfilesParameters(Struct):
    app_name: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Name of the application inserting the data (e.g. if used by a CV"
                " parsing application)\nExemple: 'HRMatch'"
            ),
        ),
    ] = None


class ArchiveProfilesParameters(Struct):
    vacancy_id: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The ID of the offer to assign the candidate to\nEquivalent to id in"
                " the Flatchr API not vacancy_id nor the slug"
            ),
        ),
    ] = None


class ReadJobsParameters(Struct):
    pass


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    FLATCHR_CANDIDATES_ENDPOINT = (
        "{base_ur}/company/{companyId}/search/applicants".format(
            base_ur=auth_parameters.env_base_url, companyId=auth_parameters.company_id
        )
    )
    headers = {"Authorization": "Bearer {}".format(auth_parameters.api_key)}
    data = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    response = requests.post(
        url=FLATCHR_CANDIDATES_ENDPOINT, headers=headers, json=data
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to fetch profiles from Flatchr status_code={} response={}".format(
                response.status_code, response.text
            )
        )
        raise Exception("Failed to fetch profiles from Flatchr")
    profiles = response.json()
    # The following steps are a workaround around limitations and misconfigurations in the Flatchr API
    # We go through the collumns endpoint to get the id of the column the candidate is in right now using the column name
    columns_response = requests.get(
        "{base_url}/company/{companyId}/columns".format(
            base_url=auth_parameters.env_base_url, companyId=auth_parameters.company_id
        ),
        headers=headers,
    )
    columns = columns_response.json()
    # we go through the vacancies endpoint to get the id of the vacancy the candidate is attached to using the vacancy_id in the profile
    vacancies_response = requests.get(
        "{base_url}/company/{companyId}/vacancies".format(
            base_url=auth_parameters.env_base_url, companyId=auth_parameters.company_id
        ),
        headers=headers,
    )
    vacancies = vacancies_response.json()

    for profile in profiles:
        column_id = next(
            (
                column["id"]
                for column in columns
                if column["title"] == profile["column"]
            ),
            None,
        )
        vacancy_id = next(
            (
                vacancy["id"]
                for vacancy in vacancies
                if vacancy["vacancy_id"] == profile["vacancy_id"]
            ),
            None,
        )
        # We move candidate to the same column he's in right now just to get the candidate id
        if column_id and vacancy_id:
            move_candidate_response = requests.put(
                "{base_url}/company/{companyId}/vacancy/{vacancyId}/applicant/{applicantId}"
                .format(
                    base_url=auth_parameters.env_base_url,
                    companyId=auth_parameters.company_id,
                    vacancyId=vacancy_id,
                    applicantId=profile["applicant"],
                ),
                headers=headers,
                json={"column_id": column_id},
            )
            if move_candidate_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to move candidate in Flatchr status_code={} response={}"
                    .format(
                        move_candidate_response.status_code,
                        move_candidate_response.text,
                    )
                )
            candidate_additional_info = move_candidate_response.json()["candidate"]
            # Get the candidate resume
            candidate_resume_response = requests.get(
                "{base_url}/company/{companyId}/files/expire?key={key}&extension={extension}&token={token}"
                .format(
                    base_url=auth_parameters.env_base_url,
                    companyId=auth_parameters.company_id,
                    key=candidate_additional_info["aws"]["key"],
                    extension=candidate_additional_info["aws"]["extension"],
                    token=auth_parameters.api_key,
                )
            )
            if candidate_resume_response.status_code // 100 != 2:
                adapter.error(
                    "Failed to get candidate resume in Flatchr status_code={}"
                    " response={}".format(
                        candidate_resume_response.status_code,
                        candidate_resume_response.text,
                    )
                )
                candidate_additional_info["resume"] = None
            else:
                candidate_additional_info["resume"] = {
                    "raw": candidate_resume_response.content,
                    "content_type": candidate_additional_info["aws"]["extension"],
                }
            profile.update(candidate_additional_info)
            yield profile
        else:
            adapter.error(
                "Failed to get column id or vacancy id for candidate column_id={} "
                "vacancy_id={}".format(column_id, vacancy_id)
            )
            raise Exception("Failed to get column id or vacancy id for candidate")


def write(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    FLATCHR_POST_CANDIDATES_ENDPOINT = "{base_url}/vacancy/candidate/json".format(
        base_url=FLATCHR_CAREERS_BASE_URL
    )

    headers = {"Authorization": "Bearer {}".format(auth_parameters.api_key)}

    for profile in items:
        profile_creation_json = dict(
            **profile, vacancy=parameters.vacancy_slug, token=auth_parameters.api_key
        )
        response = requests.post(
            FLATCHR_POST_CANDIDATES_ENDPOINT,
            headers=headers,
            json=profile_creation_json,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create profile in Flatchr status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            failed_profiles.append(profile)

    return failed_profiles


def archive(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ArchiveProfilesParameters,
    items: t.Iterable[dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    headers = {"Authorization": "Bearer {}".format(auth_parameters.api_key)}

    for profile in items:
        if parameters.vacancy_id:
            vacancy_id = parameters.vacancy_id
        else:
            # we go through the vacancies endpoint to get the id of the vacancy the candidate is attached to using the vacancy_id in the profile
            vacancies_response = requests.get(
                "{base_url}/company/{companyId}/vacancies".format(
                    base_url=auth_parameters.env_base_url,
                    companyId=auth_parameters.company_id,
                ),
                headers=headers,
            )
            vacancies = vacancies_response.json()
            vacancy_id = next(
                (
                    vacancy["id"]
                    for vacancy in vacancies
                    if vacancy["vacancy_id"] == profile["vacancy_id"]
                ),
                None,
            )

        archive_candidate_response = requests.delete(
            "{base_url}/company/{companyId}/vacancy/{vacancyId}/applicant/{applicantId}"
            .format(
                base_url=auth_parameters.env_base_url,
                companyId=auth_parameters.company_id,
                vacancyId=vacancy_id,
                applicantId=profile["applicant_id"],
            ),
            headers=headers,
        )
        if archive_candidate_response.status_code // 100 != 2:
            adapter.error(
                "Failed to archive profile in Flatchr status_code={} response={}"
                .format(
                    archive_candidate_response.status_code,
                    archive_candidate_response.text,
                )
            )
            failed_profiles.append(profile)

    return failed_profiles


def update(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateProfilesParameters,
    items: t.Iterable[dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    FLATCHR_UPDATE_CANDIDATES_ENDPOINT = (
        "{base_url}/company/{companyID}/search/candidate".format(
            base_url=auth_parameters.env_base_url, companyID=auth_parameters.company_id
        )
    )
    headers = {"Authorization": "Bearer {}".format(auth_parameters.api_key)}

    for profile in items:
        json_data = dict(reference=profile.pop("id"), type="applicants", value=profile)
        response = requests.post(
            url=FLATCHR_UPDATE_CANDIDATES_ENDPOINT,
            headers=headers,
            json=json_data,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to update profile in Flatchr status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            failed_profiles.append(profile)

    return failed_profiles


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    FLATCHR_VACANCIES_ENDPOINT = "{base_ur}/company/{companyId}/vacancies".format(
        base_ur=auth_parameters.env_base_url, companyId=auth_parameters.company_id
    )
    headers = {"Authorization": "Bearer {}".format(auth_parameters.api_key)}
    response = requests.get(url=FLATCHR_VACANCIES_ENDPOINT, headers=headers)

    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to fetch jobs from Flatchr status_code={} response={}".format(
                response.status_code, response.text
            )
        )
        raise Exception("Failed to fetch jobs from Flatchr")
    jobs = response.json()
    for job in jobs:
        yield job


JobsAisle = Aisle(
    name=Entity.job,
    schema=FlatchrVacancy,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(create=read_jobs, update=read_jobs, archive=read_jobs),
        endpoints=Endpoints(
            create=Endpoint(
                name="Get Vacancies",
                description="search for one or more vacancies",
                url="https://developers.flatchr.io/docs/QuickStart/Annonces/Recuperer_l_ensemble_de_vos_annonces",
            ),
        ),
    ),
)
ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=FlatchrProfile,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read_profiles, update=read_profiles, archive=read_profiles
        ),
        endpoints=Endpoints(
            create=Endpoint(
                name="Get Candidates",
                description="search for one or more candidates",
                url="https://developers.flatchr.io/docs/QuickStart/Candidats/Recuperer_un_candidat",
            ),
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=UpdateProfilesParameters,
            archive=ArchiveProfilesParameters,
        ),
        function=merge(create=write, update=update, archive=archive),
        endpoints=Endpoints(
            create=Endpoint(
                name="Create Candidate",
                description="Create a candidate",
                url="https://developers.flatchr.io/docs/QuickStart/Candidats/Creer_un_candidat",
            ),
            update=Endpoint(
                name="Update Candidate",
                description="Update a candidate",
                url="https://developers.flatchr.io/docs/QuickStart/Candidats/Meta_informations_candidat",
            ),
            archive=Endpoint(
                name="Archive Candidate",
                description="Archive a candidate",
                url="https://developers.flatchr.io/docs/QuickStart/Candidats/Archiver_un_candidat",
            ),
        ),
    ),
)
