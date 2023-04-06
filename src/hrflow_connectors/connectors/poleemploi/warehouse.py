import typing as t
from enum import Enum, IntEnum
from logging import LoggerAdapter

import requests
from pydantic import Field, validator

from hrflow_connectors.connectors.poleemploi.schemas import (
    ExperienceRequirement,
    OfferOriginTag,
    PoleEmploiJobOffer,
    validate_date,
)
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
)
from hrflow_connectors.core.warehouse import ActionEndpoints

POLEEMPLOI_JOBS_SEARCH_ENDPOINT = (
    "https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search"
)

POLEEMPLOI_REFERENCES_ENDPOINT = (
    "https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/"
)
SEARCH_JOBS_ENDPOINT = ActionEndpoints(
    name="Get all jobs",
    description=(
        "Endpoint to search offers based on selection criteria. The list of returned"
        " offers is paginated.The range of results is limited to 150.The request method"
        " is `GET`"
    ),
    url=(
        "https://pole-emploi.io/data/api/offres-emploi?tabgroup-api="
        "documentation&doc-section=api-doc-section-rechercher-par-crit%C3%A8res"
    ),
)
TOKEN_GENERATOR_URL = (
    "https://entreprise.pole-emploi.fr/connexion/oauth2/access_token?realm=/partenaire"
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
Note: In all cases, if originOffer = 1, then only the Pole Emploi offers
will be returned"""


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


class ReadJobsParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="Client ID used to access Pole Emploi API",
        repr=False,
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        description="Client Secret used to access Pole Emploi API",
        repr=False,
        field_type=FieldType.Auth,
    )
    range: t.Optional[str] = Field(
        description="Pagination of data. The range of results is limited to 150.",
        field_type=FieldType.QueryParam,
    )
    sort: t.Optional[int] = Field(
        description="Sorting of data",
        field_type=FieldType.QueryParam,
    )
    domaine: t.Optional[str] = Field(
        description=(
            "Professional field code"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            " to this endpoint :"
            f"{POLEEMPLOI_REFERENCES_ENDPOINT}/domaines"
        ),
        field_type=FieldType.QueryParam,
    )
    codeROME: t.Optional[str] = Field(
        description=(
            "ROME code of the profession"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            " to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/metiers"
        ),
        field_type=FieldType.QueryParam,
    )
    theme: t.Optional[str] = Field(
        description=(
            "Theme of the profession"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            " to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/themes"
        ),
        field_type=FieldType.QueryParam,
    )
    appellation: t.Optional[str] = Field(
        description=(
            "Code of the appellation"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            " to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/appellations"
        ),
        field_type=FieldType.QueryParam,
    )
    secteurActivite: t.Optional[str] = Field(
        description=(
            "NAF codes for sectors of activity. It is possible to specify two NAF codes"
            " by separating them with a comma in the character string."
            "Example : 01,02"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            " to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/secteursActivites"
        ),
        field_type=FieldType.QueryParam,
    )
    experience: t.Optional[Experience] = Field(
        description=(
            "Level of experience required\nPossible values:\n1 -> Less than 1 year of"
            " experience\n2 -> From 1 to 3 years of experience\n3 -> More than 3 years"
            " of experience"
        ),
        field_type=FieldType.QueryParam,
    )
    typeContrat: t.Optional[str] = Field(
        description=(
            "Contract type code"
            "Example : CDI,CDD"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            " to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/typesContrats"
        ),
        field_type=FieldType.QueryParam,
    )
    natureContrat: t.Optional[str] = Field(
        description=(
            "Code of the nature of contract"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            " to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/naturesContrats"
        ),
        field_type=FieldType.QueryParam,
    )
    origineOffre: t.Optional[OfferOriginTag] = Field(
        description=(
            "Origin of the offer\nPossible values:\n1 -> Job center\n2 -> Partner"
        ),
        field_type=FieldType.QueryParam,
    )
    qualification: t.Optional[Qualification] = Field(
        description=(
            "Qualification Code\nPossible values:\n0 -> Non-executive\n9 -> Executive"
        ),
        field_type=FieldType.QueryParam,
    )
    tempsPlein: t.Optional[bool] = Field(
        description=(
            "Promote the use of the WeeklyDuration filter\nPossible values:\nfalse ->"
            " Part-time\ntrue -> Full time\nIf the parameter is not filled, then all"
            " the offers are returned"
        ),
        field_type=FieldType.QueryParam,
    )
    commune: t.Optional[str] = Field(
        description=(
            "INSEE code of the commune"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/communes"
        ),
        field_type=FieldType.QueryParam,
    )
    distance: t.Optional[int] = Field(
        description=(
            "Kilometric distance of the search radius\nDefault value: 10Note: to obtain"
            " only the offers of a specific commune, then you must fill in the"
            " parameter 'distance=0'."
        ),
        field_type=FieldType.QueryParam,
    )
    # distance=0 pour pour obtenir seulement les offres d'une commune spécifique
    departement: t.Optional[str] = Field(
        description=(
            "INSEE code of the department"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/departements"
        ),
        field_type=FieldType.QueryParam,
    )
    inclureLimitrophes: t.Optional[bool] = Field(
        description="Include bordering departments in the search",
        field_type=FieldType.QueryParam,
    )
    region: t.Optional[str] = Field(
        description=(
            "Code of the region of the offer"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/regions"
        ),
        field_type=FieldType.QueryParam,
    )
    paysContinent: t.Optional[str] = Field(
        description=(
            "Code of the country or continent of the offer"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/pays"
            f"AND {POLEEMPLOI_REFERENCES_ENDPOINT}/continents"
        ),
        field_type=FieldType.QueryParam,
    )
    niveauFormation: t.Optional[str] = Field(
        description=(
            "Level of education required"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/niveauxFormations"
        ),
        field_type=FieldType.QueryParam,
    )
    permis: t.Optional[str] = Field(
        description=(
            "Code of the requested license"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLEEMPLOI_REFERENCES_ENDPOINT}/permis"
        ),
        field_type=FieldType.QueryParam,
    )
    motsCles: t.Optional[str] = Field(
        description=MOTS_CLES_DESCRIPTION,
        field_type=FieldType.QueryParam,
    )
    salaireMin: t.Optional[float] = Field(
        description=(
            "Minimum wage, expressed in Euro.If this data is filled in, the code of"
            " the type of minimum wage is mandatory."
        ),
        field_type=FieldType.QueryParam,
    )  # If this field is set, then periodeSalaire is required.
    periodeSalaire: t.Optional[SalaryPeriod] = Field(
        description=(
            "Period for the calculation of the minimum wage.\nIf this data is filled"
            " in, the minimum wage is mandatory.\nPossible values:\nM -> Monthly\nA ->"
            " Annual\nH -> Hourly\nC -> Fee"
        ),
        field_type=FieldType.QueryParam,
    )  # If this field is set, then salaireMin is required.
    accesTravailleurHandicape: t.Optional[bool] = Field(
        description=(
            "Allows you to search for offers for which the employer is handi friendly"
        ),
        field_type=FieldType.QueryParam,
    )
    offresMRS: t.Optional[bool] = Field(
        description=(
            " Allows you to search for jobs that offer the simulation recruitment"
            " method"
        ),
        field_type=FieldType.QueryParam,
    )
    grandDomaine: t.Optional[IndustryDomain] = Field(
        description="Code of the major area of the offer",
        field_type=FieldType.QueryParam,
    )
    experienceExige: t.Optional[ExperienceRequirement] = Field(
        description="Filter offers by experience level.",
        field_type=FieldType.QueryParam,
    )
    publieeDepuis: t.Optional[PublishedSince] = Field(
        description=(
            "Maximum number of days since the publication of the offer\nPossible"
            " values: 1, 3, 7, 14, 31"
        ),
        field_type=FieldType.QueryParam,
    )
    minCreationDate: t.Optional[str] = Field(
        description=(
            "Minimum offer creation date.\nIf this data is filled in, the maximum offer"
            " creation date is mandatory.\nISO-8601 standard (YYYY-MM-DDTHH:MM:SSZ)"
        ),
        field_type=FieldType.QueryParam,
    )  # If this field is set, then minCreationDate is required.
    maxCreationDate: t.Optional[str] = Field(
        description=(
            "Maximum offer creation date.\nIf this data is filled in, the minimum offer"
            " creation date is mandatory.\nISO-8601 standard (YYYY-MM-DDTHH:MM:SSZ)"
        ),
        field_type=FieldType.QueryParam,
    )  # If this field is set, then maxCreationDate is required.

    partenaires: t.Optional[str] = Field(
        description=(
            " This filter allows you to enter your partner code in order to include or"
            " exclude your offers from the results according to the selectionmade in"
            " the PartnerSelection mode filter\nIt is possible to enter several codes"
            " (separator ','). "
        ),
        field_type=FieldType.QueryParam,
    )  # Il est possible de saisir plusieurs codes (séparateur ",").
    modeSelectionPartenaires: t.Optional[PartnerSelectionMode] = Field(
        description=MODESELECTIONPARTENAIRES_DESCRIPTION,
        field_type=FieldType.QueryParam,
    )  # If this field is set, then maxCreationDate is required.

    dureeHebdo: t.Optional[WeeklyDuration] = Field(
        description=(
            "Filtre les offres selon la durée hebdomadaire.\nValeurs possibles :\n0 ->"
            " Non précisé\n1 -> Temps plein\n2 -> Temps partiel"
        ),
        field_type=FieldType.QueryParam,
    )
    dureeHebdoMin: t.Optional[int] = Field(
        description=(
            "Minimum weekly duration of the offer\nThe value must be in HHMM format,"
            " for example : 8h => 800 ; 24h30 => 2430"
        ),
        field_type=FieldType.QueryParam,
    )  # format HHMM
    dureeHebdoMax: t.Optional[int] = Field(
        description=(
            "Maximum weekly duration of the offer\nThe value must be in HHMM format,"
            " for example: 8h => 800; 24h30 => 2430"
        ),
        field_type=FieldType.QueryParam,
    )  # format HHMM
    dureeContratMin: t.Optional[float] = Field(
        description=(
            "Minimum duration of the sought contract.\nThe search is done in months"
            " (ex: 0.5 for 15 days, 1.0 for 1 month,2.0 for 2 months).\nPositive"
            " decimal (Decimal separator: '.')"
        ),
        field_type=FieldType.QueryParam,
    )
    dureeContratMax: t.Optional[float] = Field(
        description=(
            "Maximum duration of the sought contract.\nThe search is made in months"
            " (ex: 0.5 for 15 days, 1.0 for 1 month,2.0 for 2 months).\nPositive"
            " decimal (Decimal separator: '.')"
        ),
        field_type=FieldType.QueryParam,
    )
    offresManqueCandidats: t.Optional[bool] = Field(
        description=(
            "Filters offers older than 15 days, with less than 4 applications (of which"
            " Pôle emploi is informed)\nfalse -> Offers not concerned\ntrue -> Offers"
            " with few candidates"
        ),
        field_type=FieldType.QueryParam,
    )
    entreprisesAdaptees: t.Optional[bool] = Field(
        description=(
            "Filter the offers where the adapted company allows a disabled worker to"
            " exercise a professional activity in  conditions adapted to his"
            " capacities\nfalse -> Offers not concerned\ntrue -> Offers from adapted"
            " companies"
        ),
        field_type=FieldType.QueryParam,
    )

    # validators
    _validate_minCreationDate = validator("minCreationDate", allow_reuse=True)(
        validate_date
    )
    _validate_maxCreationDate = validator("maxCreationDate", allow_reuse=True)(
        validate_date
    )


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
    )
    if not response.ok:
        raise Exception(
            "Failed to get authentication token with error={}".format(response.text)
        )
    try:
        return response.json()["access_token"]
    except (KeyError, requests.exceptions.JSONDecodeError) as e:
        raise Exception(
            "Failed to get token from response with error={}".format(repr(e))
        )


def read(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token = get_poleemploi_auth_token(
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
    )
    params = parameters.dict(exclude_none=True)
    del params["client_id"]
    del params["client_secret"]

    response = requests.get(
        POLEEMPLOI_JOBS_SEARCH_ENDPOINT,
        headers={"Authorization": "Bearer {}".format(token)},
        params=params,
    )
    if response.status_code // 100 != 2:
        adapter.error(
            "Failed to pull jobs from Pole Emploi params={}"
            " status_code={} response={}".format(
                params, response.status_code, response.text
            )
        )
        raise Exception("Failed to pull jobs from Pole Emploi")
    jobs = response.json()["resultats"]
    for job in jobs:
        yield job


PoleEmploiJobWarehouse = Warehouse(
    name="Pole Emploi Jobs",
    data_schema=PoleEmploiJobOffer,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[SEARCH_JOBS_ENDPOINT],
    ),
)
