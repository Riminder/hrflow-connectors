import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.flatchr.schemas import FlatchrCreationProfile
from hrflow_connectors.core import (
    ActionEndpoints,
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseWriteAction,
)

FLATCHR_PROFILE_ENDPOINT = "https://careers.flatchr.io/"
POST_CANDIDATE_ENDPOINT = ActionEndpoints(
    name="Post Candidate",
    description=(
        "Endpoint to create a new candidate and assign to a talent pool, the request"
        " method is `POST`"
    ),
    url="https://developers.flatchr.io/docs/QuickStart/Candidats/Creer_un_candidat",
)


class WriteProfilesParameters(ParametersModel):
    auth: str = Field(
        ...,
        description=(
            "Auth used to authenticate with an API key named `Authorization` in the"
            " headers (hash key), go to Paramètres avancée -> Avancé -> API"
        ),
        field_type=FieldType.Auth,
    )
    vacancy: str = Field(
        ...,
        description=(
            "The pool in which candidates will be placed. Findable in the URL of the"
            " job to which you want to add a candidate"
        ),
        field_type=FieldType.QueryParam,
    )
    company: str = Field(
        ..., description="The id of the company", field_type=FieldType.QueryParam
    )


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} profiles".format(len(profiles)))
    failed_profiles = []
    for profile in profiles:
        create_profile_body = profile["create_profile_body"]
        enrich_profile_body = profile["enrich_profile_body"]
        create_profile_body["vacancy"] = parameters.vacancy
        create_profile_body["token"] = parameters.auth
        response = requests.post(
            url=(FLATCHR_PROFILE_ENDPOINT + "vacancy/candidate/json"),
            headers={"Authorization": "Bearer " + parameters.auth},
            json=create_profile_body,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile to Flatchr status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)

        adapter.info("Sending `enrich profile` request")
        # the 2 following request is for getting cadidate id number
        # In documentation for 'ajouter des metas informations' request,
        # it is written the 'reference' field is the mail of the candidate,
        # but in practice it is the candidate id number.I told Flatchr the problem,
        # they said they are patching it. If eventually it accept mail,
        # remove those 2 requests

        # 'Récupérer les informations d'un candidat' request
        response_info = requests.post(
            url="http://api.flatchr.io/company/{}/search/applicants".format(
                parameters.company
            ),
            headers={"Authorization": "Bearer " + parameters.auth},
            json={"lastname": create_profile_body["lastname"]},
        )
        response_info = response_info.json()
        applicant_id = response_info[0]["applicant"]
        # 'Déplacer un candidat de colonne' request to get candidate id number.
        #  (we are not actually moving the candidate, but candidate id in response)
        response_move_candidat = requests.put(
            url="http://api.flatchr.io/company/{}/vacancy/{}/applicant/{}".format(
                parameters.company, parameters.vacancy, applicant_id
            ),
            headers={"Authorization": "Bearer " + parameters.auth},
            json={},
        )
        response_move_candidat = response_move_candidat.json()
        candidate_id = response_move_candidat["candidate"]["consent"][0]["candidate_id"]

        enrich_profile_body["reference"] = candidate_id
        # enrichment request
        response = requests.post(
            url="http://api.flatchr.io/company/{}/search/candidate".format(
                parameters.company
            ),
            headers={"Authorization": "Bearer " + parameters.auth},
            json=enrich_profile_body,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to enrich profile to Flatchr status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)

    return failed_profiles


FlatchrProfileWarehouse = Warehouse(
    name="Flatchr Profiles",
    data_schema=FlatchrCreationProfile,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
        endpoints=[POST_CANDIDATE_ENDPOINT],
    ),
)
