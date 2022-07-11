import typing as t
from enum import Enum, IntEnum
from logging import LoggerAdapter

import requests
from pydantic import BaseModel, Field, validator

from hrflow_connectors.connectors.poleemploi.referentials import (
    Listofdepartments,
    Listofdomaines,
    ListofpaysContinents,
    Listofpermis,
    Listofregions,
    Listofthemes,
)
from hrflow_connectors.connectors.poleemploi.schemas import (
    Appellation,
    CodeROME,
    Commune,
    ExperienceExige,
    NatureContrat,
    NiveauFormation,
    OrigineOffreTag,
    PoleEmploiJobOffer,
    SecteurActivite,
    TypeContrat,
    validate_date,
)
from hrflow_connectors.core import Warehouse, WarehouseReadAction
from hrflow_connectors.core.warehouse import ActionEndpoints

POLEEMPLOI_JOBS_SEARCH_ENDPOINT = (
    "https://api.emploi-store.fr/partenaire/offresdemploi/v2/offres/search"
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


class JobLocation(BaseModel):
    libelle: str
    latitude: float
    longitude: float
    codePostal: str


Domaine = Enum("Domaine", dict(Listofdomaines), type=str)
Theme = Enum("Theme", dict(Listofthemes), type=str)
Region = Enum("Region", dict(Listofregions), type=str)
PaysContinent = Enum("PaysContinent", dict(ListofpaysContinents), type=str)
Permis = Enum("Permis", dict(Listofpermis), type=str)
Departement = Enum("Departement", dict(Listofdepartments), type=str)


class Experience(str, Enum):
    lessthanoneyear = "1"  # 1 -> Moins d'un an d'expérience
    lessthanthree = "2"  # 2 -> De 1 à 3 ans d'expérience
    morethanthree = "3"  # 3 -> Plus de 3 ans d'expérience


class Qualification(IntEnum):
    nonCadre = 0
    cadre = 9


class PeriodeSalaire(str, Enum):
    Mensuel = "M"
    Annuel = "A"
    Horaire = "H"
    Cachet = "C"


class GrandDomaine(str, Enum):
    agriculture = (  # A -> Agriculture/Pêche/
        # /Espaces verts et naturels/Soins aux animaux
        "A"
    )
    arts = "B"  # B -> Arts / Artisanat d’art
    banque = "C"  # C -> Banque / Assurance
    immobilier = "C15"  # C15 -> Immobilier
    commerce = "D"  # D -> Commerce / Vente
    communication = "E"  # E -> Communication / Multimédia
    batiment = "F"  # F -> Bâtiment / Travaux Publics
    hotellerie = "G"  # G -> Hôtellerie – Restauration / Tourisme / Animation
    industrie = "H"  # H -> Industrie
    installation = "I"  # I -> Installation / Maintenance
    sante = "J"  # J -> Santé
    services = "K"  # K -> Services à la personne / à la collectivité
    spectacle = "L"  # L -> Spectacle
    sport = "L14"  # L14 -> Sport
    achats = "M"  # M -> Achats / Comptabilité / Gestion
    direction = "M13"  # M13 -> Direction d’entreprise
    conseil = "M14"  # M14 -> Conseil/Etudes
    ressources = "M15"  # M15 -> Ressources Humaines
    secretariat = "M16"  # M16 -> Secrétariat/Assistanat
    marketing = "M17"  # M17 -> Marketing /Stratégie commerciale
    informatique = "M18"  # M18 -> Informatique / Télécommunication
    transport = "N"  # N -> Transport / Logistique


class PublieeDepuis(IntEnum):
    # Valeurs possibles : 1, 3, 7, 14, 31
    a = 1
    b = 3
    c = 7
    d = 14
    e = 31


class ModeSelectionPartenaires(str, Enum):
    inclus = "INCLUS"
    exclu = "EXCLU"


class DureeHebdo(str, Enum):
    nonPrecise = "0"
    tempsPlein = "1"
    tempsPartiel = "2"


class ReadJobsParameters(BaseModel):
    client_id: str = Field(
        ..., description="Client ID used to access Pole Emploi API", repr=False
    )
    client_secret: str = Field(
        ..., description="Client Secret used to access Pole Emploi API", repr=False
    )
    range: t.Optional[str]
    sort: t.Optional[int]
    domaine: t.Optional[Domaine]
    codeROME: t.Optional[CodeROME]
    theme: t.Optional[Theme]
    appellation: t.Optional[Appellation]
    secteurActivite: t.Optional[SecteurActivite]
    experience: t.Optional[Experience]
    typeContrat: t.Optional[TypeContrat]
    natureContrat: t.Optional[NatureContrat]
    origineOffre: t.Optional[OrigineOffreTag]
    qualification: t.Optional[Qualification]
    tempsPlein: t.Optional[bool]
    commune: t.Optional[Commune]
    distance = (
        10  # distance=0 pour pour obtenir seulement les offres d'une commune spécifique
    )
    departement: t.Optional[Departement]
    inclureLimitrophes: t.Optional[bool]
    region: t.Optional[Region]
    paysContinent: t.Optional[PaysContinent]
    niveauFormation: t.Optional[NiveauFormation]
    permis: t.Optional[Permis]
    motsCles: t.Optional[str]
    salaireMin: t.Optional[float]
    periodeSalaire: t.Optional[
        PeriodeSalaire
    ]  # TODO: link salaireMin w/ periodeSalaire
    accesTravailleurHandicape: t.Optional[bool]
    offresMRS: t.Optional[bool]
    grandDomaine: t.Optional[GrandDomaine]
    experienceExigence: t.Optional[ExperienceExige]
    publieeDepuis: t.Optional[PublieeDepuis]
    minCreationDate: t.Optional[str]
    maxCreationDate: t.Optional[str]

    partenaires: t.Optional[
        str
    ]  # Il est possible de saisir plusieurs codes (séparateur ",").
    modeSelectionPartenaires: t.Optional[ModeSelectionPartenaires]
    dureeHebdo: t.Optional[DureeHebdo]
    dureeHebdoMin: t.Optional[int]  # TODO : format HHMM
    dureeHebdoMax: t.Optional[int]  # TODO : format HHMM
    dureeContratMin: t.Optional[
        float
    ]  # TODO : Décimal positif (Séparateur de décimales: '.')
    dureeContratMax: t.Optional[
        float
    ]  # TODO : Décimal positif (Séparateur de décimales: '.')
    offresManqueCandidats: t.Optional[bool]
    entreprisesAdaptees: t.Optional[bool]

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


def read(adapter: LoggerAdapter, parameters: ReadJobsParameters) -> t.Iterable[t.Dict]:
    token = get_poleemploi_auth_token(
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
    )
    params = parameters.dict()
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
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read,
        endpoints=[SEARCH_JOBS_ENDPOINT],
    ),
)
