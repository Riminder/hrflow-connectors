import typing as t
from enum import Enum, IntEnum
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec import json as msgspec_json
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.francetravail.schemas import (
    ExperienceRequirement,
    FranceTravailJobOffer,
    OfferOriginTag,
    validate_date,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    Endpoint,
    Endpoints,
    ReadOperation,
    merge,
)

API_BASE_URL = "https://api.francetravail.io/partenaire/offresdemploi"
JOBS_SEARCH_URL = ("{}/v2/offres/search").format(API_BASE_URL)

REFERENCES_URL = ("{}/v2/referentiel/").format(API_BASE_URL)

SEARCH_JOBS_ENDPOINT = Endpoint(
    name="Get all jobs",
    description=(
        "Endpoint to search offers based on selection criteria. The list of returned"
        " offers is paginated.The range of results is limited to 150.The request method"
        " is `GET`"
    ),
    url=JOBS_SEARCH_URL,
)
TOKEN_GENERATOR_URL = (
    "https://entreprise.francetravail.fr/connexion/oauth2/access_token"
)

GRANT_TYPE = "client_credentials"
TOKEN_SCOPE = "api_offresdemploiv2 o2dsoffre"

MOTS_CLES_DESCRIPTION = """Search by keyword

Each keyword (or expression) is at least 2 characters long and must
be separated by a comma.
The search on several keywords is processed via the logical operator "AND".
The keyword search can be used to search on :

- The title of the offer (title field in the search return)
- The ROME code (romeCode field in the search return)
- The ROME label (field romeLibelle in return for the search)
- The competences label (field competences.libelle in return of the search)
- The wording of the training fields (field formations.domaineLibelle in
return of the research)
- The wording of the permits (field permits.label in return of the search)
- The language label (field languages.label in return of the search)
- The offer description if found in the offer title and/or the ROME label
(description field in the search return)

Allowed characters: [aA-zZ]+[0-9]+[space]+[@#$%^&+./-""]"""

MODESELECTIONPARTENAIRES_DESCRIPTION = """Selection mode of the partner offers.

This filter works with the partner criterion and is dependent on the originOffer
criterion. Possible values with the results obtained according to the two other filters:

- INCLUS(INCLUDED)
originOffer empty : Returns the PE offers and the Partners listed in the Partners
criterion
originOffer at 2 : Only the offers of the Partners listed in the Partners
criterion
- EXCLU(EXCLUDED)
originOffer empty : Return the offers of PE and Partners not listed in the Partners
criterion
originOffer at 2 : Only the offers of the Partners not listed in the Partners
criterion
Note: In all cases, if originOffer = 1, then only the France Travail (ex: Pole Emploi)
offers will be returned"""


class Experience(str, Enum):
    LESS_THAN_ONE_YEAR = "1"
    ONE_TO_THREE_YEARS = "2"
    MORE_THAN_THREE_YEARS = "3"


class Qualification(IntEnum):
    NON_EXECUTIVE = 0
    EXECUTIVE = 9


class SalaryPeriod(str, Enum):
    MONTHLY = "M"
    ANNUALLY = "A"
    HOURLY = "H"
    FEE_FOR_SERVICE = "C"


class IndustryDomain(str, Enum):
    AGRICULTURE = "A"
    ARTS = "B"
    BANKING_INSURANCE = "C"
    REAL_ESTATE = "C15"
    RETAIL = "D"
    MEDIA_COMMUNICATION = "E"
    CONSTRUCTION = "F"
    HOSPITALITY_TOURISM = "G"
    INDUSTRY = "H"
    INSTALLATION_MAINTENANCE = "I"
    HEALTHCARE = "J"
    PERSONAL_COMMUNITY_SERVICES = "K"
    PERFORMING_ARTS = "L"
    SPORT = "L14"
    PURCHASING_ACCOUNTING_MANAGEMENT = "M"
    EXECUTIVE_MANAGEMENT = "M13"
    CONSULTING_RESEARCH = "M14"
    HUMAN_RESOURCES = "M15"
    SECRETARIAL_ASSISTANTSHIP = "M16"
    MARKETING_SALES_STRATEGY = "M17"
    COMPUTER_TELECOMMUNICATION = "M18"
    TRANSPORTATION_LOGISTICS = "N"


class PublishedSince(IntEnum):
    ONE_DAY = 1
    THREE_DAYS = 3
    SEVEN_DAYS = 7
    FOURTEEN_DAYS = 14
    THIRTY_ONE_DAYS = 31


class PartnerSelectionMode(str, Enum):
    INCLUDE = "INCLUS"
    EXCLUDE = "EXCLU"


class WeeklyDuration(str, Enum):
    NOT_SPECIFIED = "0"
    FULL_TIME = "1"
    PART_TIME = "2"


class AuthParameters(Struct):
    client_id: Annotated[
        str,
        Meta(
            description="Client ID used to access France Travail (ex: Pole Emploi) API",
        ),
    ]
    client_secret: Annotated[
        str,
        Meta(
            description=(
                "Client Secret used to access France Travail (ex: Pole Emploi) API"
            ),
        ),
    ]


class ReadJobsParameters(Struct, omit_defaults=True):
    range: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Pagination of data. The range of results is limited to 150. Format:"
                " p-d, where :\n\np is the index (starting at 0) of the first element"
                " requested, which must not exceed 3000\nd is the index of the last"
                " element requested, which must not exceed 3149"
            ),
        ),
    ] = None
    sort: Annotated[
        t.Optional[int],
        Meta(
            description="Sorting of data",
        ),
    ] = None
    domaine: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Professional field code"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                " to this endpoint :"
                f"{REFERENCES_URL}/domaines"
            ),
        ),
    ] = None
    codeROME: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "ROME code of the profession"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                " to this endpoint :"
                f" {REFERENCES_URL}/metiers"
            ),
        ),
    ] = None
    theme: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Theme of the profession"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                " to this endpoint :"
                f" {REFERENCES_URL}/themes"
            ),
        ),
    ] = None
    appellation: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "ROME designation code for the offer, see reference below"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                " to this endpoint :"
                f" {REFERENCES_URL}/appellations"
            ),
        ),
    ] = None
    codeNAF: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "NAF code of the offer (format 99.99X)"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                " to this endpoint :"
                f" {REFERENCES_URL}/nafs"
            ),
        ),
    ] = None
    secteurActivite: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "NAF codes for sectors of activity. It is possible to specify two NAF"
                " codes by separating them with a comma in the character"
                " string.Example : 01,02A GET request for the list of accepted choices"
                " from the Offres d'emploi API to this endpoint :"
                f" {REFERENCES_URL}/secteursActivites"
            ),
        ),
    ] = None
    experience: Annotated[
        t.Optional[Experience],
        Meta(
            description=(
                "Level of experience required\nPossible values:\n1 -> Less than 1 year"
                " of experience\n2 -> From 1 to 3 years of experience\n3 -> More than 3"
                " years of experience"
            ),
        ),
    ] = None
    experienceExigence: Annotated[
        t.Optional[ExperienceRequirement],
        Meta(
            description=(
                "Filter offers by experience requirement (D beginner accepted, S"
                " experience desired, E experience required)"
            ),
        ),
    ] = None
    typeContrat: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Contract type code"
                "Example : CDI,CDD"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                " to this endpoint :"
                f" {REFERENCES_URL}/typesContrats"
            ),
        ),
    ] = None
    natureContrat: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Code of the nature of contract"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                " to this endpoint :"
                f" {REFERENCES_URL}/naturesContrats"
            ),
        ),
    ] = None
    origineOffre: Annotated[
        t.Optional[OfferOriginTag],
        Meta(
            description=(
                "Origin of the offer\nPossible values:\n1 -> Job center\n2 -> Partner"
            ),
        ),
    ] = None
    qualification: Annotated[
        t.Optional[Qualification],
        Meta(
            description=(
                "Qualification Code\nPossible values:\n0 -> Non-executive\n9 ->"
                " Executive"
            ),
        ),
    ] = None
    tempsPlein: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "Promote the use of the WeeklyDuration filter\nPossible values:\nfalse"
                " -> Part-time\ntrue -> Full time\nIf the parameter is not filled, then"
                " all the offers are returned"
            ),
        ),
    ] = None
    commune: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "INSEE code of the commune"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {REFERENCES_URL}/communes"
            ),
        ),
    ] = None
    distance: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Kilometric distance of the search radius\nDefault value: 10Note: to"
                " obtain only the offers of a specific commune, then you must fill in"
                " the parameter 'distance=0'."
            ),
        ),
    ] = None
    # distance=0 pour pour obtenir seulement les offres d'une commune spécifique
    departement: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Job department"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {REFERENCES_URL}/departements"
            ),
        ),
    ] = None
    inclureLimitrophes: Annotated[
        t.Optional[bool],
        Meta(
            description="Include bordering departments in the search",
        ),
    ] = None
    region: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Code of the region of the offer"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {REFERENCES_URL}/regions"
            ),
        ),
    ] = None
    paysContinent: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Code of the country or continent of the offer"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {REFERENCES_URL}/pays"
                f"AND {REFERENCES_URL}/continents"
            ),
        ),
    ] = None
    niveauFormation: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Level of education required"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {REFERENCES_URL}/niveauxFormations"
            ),
        ),
    ] = None
    permis: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Code of the requested license"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {REFERENCES_URL}/permis"
            ),
        ),
    ] = None
    motsCles: Annotated[
        t.Optional[str],
        Meta(
            description=MOTS_CLES_DESCRIPTION,
        ),
    ] = None
    salaireMin: Annotated[
        t.Optional[float],
        Meta(
            description=(
                "Minimum wage, expressed in Euro.If this data is filled in, the code of"
                " the type of minimum wage is mandatory."
            ),
        ),
    ] = None  # If this field is set, then periodeSalaire is required.
    periodeSalaire: Annotated[
        t.Optional[SalaryPeriod],
        Meta(
            description=(
                "Period for the calculation of the minimum wage.\nIf this data is"
                " filled in, the minimum wage is mandatory.\nPossible values:\nM ->"
                " Monthly\nA -> Annual\nH -> Hourly\nC -> Fee"
            ),
        ),
    ] = None  # If this field is set, then salaireMin is required.
    accesTravailleurHandicape: Annotated[
        t.Optional[bool],
        Meta(
            description="Jobs for which the employer is disabled-friendly",
        ),
    ] = None
    offresMRS: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                " Allows you to search for jobs that offer the simulation recruitment"
                " method"
            ),
        ),
    ] = None
    grandDomaine: Annotated[
        t.Optional[IndustryDomain],
        Meta(
            description="Code of the major area of the offer",
        ),
    ] = None
    experienceExige: Annotated[
        t.Optional[ExperienceRequirement],
        Meta(
            description="Filter offers by experience level.",
        ),
    ] = None
    publieeDepuis: Annotated[
        t.Optional[PublishedSince],
        Meta(
            description=(
                "Maximum number of days since the publication of the offer\nPossible"
                " values: 1, 3, 7, 14, 31"
            ),
        ),
    ] = None
    minCreationDate: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Minimum offer creation date.\nIf this data is filled in, the maximum"
                " offer creation date is mandatory.\nISO-8601 standard"
                " (YYYY-MM-DDTHH:MM:SSZ)"
            ),
        ),
    ] = None  # If this field is set, then minCreationDate is required.
    maxCreationDate: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Maximum offer creation date.\nIf this data is filled in, the minimum"
                " offer creation date is mandatory.\nISO-8601 standard"
                " (YYYY-MM-DDTHH:MM:SSZ)"
            ),
        ),
    ] = None  # If this field is set, then maxCreationDate is required.

    partenaires: Annotated[
        t.Optional[str],
        Meta(
            description=(
                " This filter allows you to enter your partner code in order to include"
                " or exclude your offers from the results according to the"
                " selectionmade in the PartnerSelection mode filter\nIt is possible to"
                " enter several codes (separator ','). "
            ),
        ),
    ] = None  # Il est possible de saisir plusieurs codes (séparateur ",").
    modeSelectionPartenaires: Annotated[
        t.Optional[PartnerSelectionMode],
        Meta(
            description=MODESELECTIONPARTENAIRES_DESCRIPTION,
        ),
    ] = None  # If this field is set, then maxCreationDate is required.

    dureeHebdo: Annotated[
        t.Optional[WeeklyDuration],
        Meta(
            description=(
                "Filtre les offres selon la durée hebdomadaire.\nValeurs possibles :\n0"
                " -> Non précisé\n1 -> Temps plein\n2 -> Temps partiel"
            ),
        ),
    ] = None
    dureeHebdoMin: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Minimum weekly duration of the offer\nThe value must be in HHMM"
                " format, for example : 8h => 800 ; 24h30 => 2430"
            ),
        ),
    ] = None  # format HHMM
    dureeHebdoMax: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "Maximum weekly duration of the offer\nThe value must be in HHMM"
                " format, for example: 8h => 800; 24h30 => 2430"
            ),
        ),
    ] = None  # format HHMM
    dureeContratMin: Annotated[
        t.Optional[float],
        Meta(
            description=(
                "Minimum duration of the sought contract.\nThe search is done in months"
                " (ex: 0.5 for 15 days, 1.0 for 1 month,2.0 for 2 months).\nPositive"
                " decimal (Decimal separator: '.')"
            ),
        ),
    ] = None
    dureeContratMax: Annotated[
        t.Optional[float],
        Meta(
            description=(
                "Maximum duration of the sought contract.\nThe search is made in months"
                " (ex: 0.5 for 15 days, 1.0 for 1 month,2.0 for 2 months).\nPositive"
                " decimal (Decimal separator: '.')"
            ),
        ),
    ] = None
    offresManqueCandidats: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "Filters offers older than 15 days, with less than 4 applications (of"
                " which Pôle emploi is informed)\nfalse -> Offers not concerned\ntrue"
                " -> Offers with few candidates"
            ),
        ),
    ] = None
    entreprisesAdaptees: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "Filter the offers where the adapted company allows a disabled worker"
                " to exercise a professional activity in  conditions adapted to his"
                " capacities\nfalse -> Offers not concerned\ntrue -> Offers from"
                " adapted companies"
            ),
        ),
    ] = None

    def __post_init__(self):
        # Validate date fields
        validate_date(self.minCreationDate, "minCreationDate")
        validate_date(self.maxCreationDate, "maxCreationDate")


def get_poleemploi_auth_token(client_id: str, client_secret: str) -> str:
    response = requests.post(
        TOKEN_GENERATOR_URL,
        headers={
            "Content-Type": "application/x-www-form-urlencoded",
        },
        data=dict(
            grant_type=GRANT_TYPE,
            scope=TOKEN_SCOPE,
            client_id=client_id,
            client_secret=client_secret,
        ),
        params=dict(
            realm="/partenaire",
        ),
    )
    if response.status_code != 200:
        raise ValueError(
            "Failed to get auth token from France Travail (ex: Pole Emploi):"
            f" {response.text}"
        )
    return response.json()["access_token"]


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    token = get_poleemploi_auth_token(
        client_id=auth_parameters.client_id,
        client_secret=auth_parameters.client_secret,
    )
    params = msgspec_json.decode(msgspec_json.encode(parameters), type=dict)

    response = requests.get(
        JOBS_SEARCH_URL,
        headers={"Authorization": "Bearer {}".format(token)},
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull jobs from France Travail (ex: Pole Emploi) params={}"
            " status_code={} response={}".format(
                params, response.status_code, response.text
            )
        )
        raise Exception("Failed to pull jobs from France Travail (ex: Pole Emploi)")
    jobs = response.json()["resultats"]
    for job in jobs:
        yield job


JobsAisle = Aisle(
    name=Entity.job,
    schema=FranceTravailJobOffer,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(create=read, update=read, archive=read),
        endpoints=Endpoints(
            create=SEARCH_JOBS_ENDPOINT,
            update=SEARCH_JOBS_ENDPOINT,
            archive=SEARCH_JOBS_ENDPOINT,
        ),
    ),
)
