import datetime
import typing as t
from enum import Enum, IntEnum
from typing import Any, List

from pydantic import BaseModel, Field, validator

POLE_EMPLOI_REFERENCES_ENDPOINT = (
    "https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/"
)


def validate_date(value: Any) -> Any:
    try:
        datetime.date.fromisoformat(value)
        return value
    except ValueError:
        raise ValueError(
            f"Invalid time format given {value}, expected format ISO 8601 format"
            " ISO-8601 (YYYY-MM-DDTHH:MM:SSZ)"
        )


class JobLocation(BaseModel):
    libelle: str
    latitude: float
    longitude: float
    codepostal: str
    commune: str = Field(
        description=(
            "INSEE code of the commune"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}communes"
        ),
    )


class Entreprise(BaseModel):
    nom: t.Optional[str]
    description: t.Optional[str]
    logo: t.Optional[str]
    url: t.Optional[str]
    entrepriseAdaptee: bool


class Partner(BaseModel):
    nom: str
    url: str
    logo: str


class OfferOriginTag(IntEnum):
    POLE_EMPLOI = 1
    PARTNER = 2


class OfferOrigin(BaseModel):
    origine: OfferOriginTag
    urlOrigine: t.Optional[str]
    partenaires: List[Partner]


class ExperienceRequirement(str, Enum):
    BEGINNER_ACCEPTED = "D"
    EXPERIENCE_DESIRED = "S"
    EXPERIENCE_REQUIRED = "E"


class Exigence(Enum):
    REQUIRED = "E"
    DESIRED = "S"


class Formation(BaseModel):
    domaineLibelle: str
    niveauLibelle: str = Field(
        description=(
            "Label of the level of the education required"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}referentiel/niveauxFormations"
        ),
    )
    commentaire: str
    exigence: Exigence


class Langue(BaseModel):
    libelle: str = Field(
        description=(
            "Language label"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}referentiel/langues"
        ),
    )
    exigence: t.Optional[Exigence]


class Permis(BaseModel):
    libelle: str = Field(
        description=(
            "requested license"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}referentiel/permis"
        ),
    )
    exigence: t.Optional[Exigence]


class Competence(BaseModel):
    code: str
    libelle: str
    exigence: t.Optional[Exigence]


class Salaire(BaseModel):
    libelle: t.Optional[str]
    commentaire: t.Optional[str]
    complement1: t.Optional[str]
    complement2: t.Optional[str]


class Contact(BaseModel):
    nom: t.Optional[str]
    coordonnees1: t.Optional[str]
    coordonnees2: t.Optional[str]
    coordonnees3: t.Optional[str]
    telephone: t.Optional[str]
    courriel: t.Optional[str]
    commentaire: t.Optional[str]
    urlRecruteur: t.Optional[str]
    urlPostulation: t.Optional[str]


class Agence(BaseModel):
    telephone: t.Optional[str]
    courriel: t.Optional[str]


class QualificationCode(str, Enum):
    MANOEUVRE = "1"
    SPECIALIZED_WORKER = "2"
    QUALIFIED_WORKER_P1_P2 = "3"
    QUALIFIED_WORKER_P3_P4_OHQ = "4"
    UNQUALIFIED_EMPLOYEE = "5"
    QUALIFIED_EMPLOYEE = "6"
    TECHNICIAN = "7"
    SUPERVISORY_AGENT = "8"
    EXECUTIVE = "9"


class QualificationLibelle(str, Enum):
    MANOEUVRE = "Manœuvre"
    SPECIALIZED_WORKER = "Ouvrier spécialisé"
    QUALIFIED_WORKER_P1_P2 = "Ouvrier qualifié (P1, P2)"
    QUALIFIED_WORKER_P3_P4_OHQ = "Ouvrier qualifié (P3, P4, OHQ)"
    UNQUALIFIED_EMPLOYEE = "Employé non qualifié"
    QUALIFIED_EMPLOYEE = "Employé qualifié"
    TECHNICIAN = "Technicien"
    SUPERVISORY_AGENT = "Agent de maîtrise"
    EXECUTIVE = "Cadre"


class QualitePro(BaseModel):
    libelle: t.Optional[str]
    description: t.Optional[str]


class PoleEmploiJobOffer(BaseModel):
    id: int
    intitule: str
    description: str
    dateCreation: t.Optional[str]
    dateActualisation: t.Optional[str]
    lieuTravail: t.Optional[JobLocation]
    romeCode: t.Optional[str] = Field(
        description=(
            "ROME code of the profession"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}metiers"
        ),
    )
    romeLibelle: t.Optional[str]
    appellationLibelle: t.Optional[str] = Field(
        description=(
            "Code of the appellation"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}appellations"
        ),
    )
    entreprise: t.Optional[Entreprise]
    typeContrat: t.Optional[str] = Field(
        description=(
            "Contract type code"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}typesContrats"
        ),
    )
    typeContratLibelle: t.Optional[str] = Field(
        description=(
            "Contract type label"
            "Example : CDI,CDD"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}typesContrats"
        ),
    )
    natureContrat: t.Optional[str] = Field(
        description=(
            "Code of the nature of contract"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}naturesContrats"
        ),
    )
    origineOffre: t.Optional[OfferOrigin]
    offresManqueCandidats: t.Optional[bool]
    experienceExige: t.Optional[ExperienceRequirement]
    experienceLibelle: t.Optional[str]
    experienceCommentaire: t.Optional[str]
    formations: t.Optional[List[Formation]]
    langues: t.Optional[List[Langue]]
    permis: t.Optional[List[Permis]]
    outilsBureautiques: t.Optional[str]
    competences: t.Optional[List[Competence]]
    salaire: t.Optional[Salaire]
    dureeTravailLibelle: t.Optional[str]
    dureeTravailLibelleConverti: t.Optional[str]
    complementExercice: t.Optional[str]
    conditionExercice: t.Optional[str]
    alternance: t.Optional[bool]
    contact: t.Optional[Contact]
    agence: t.Optional[Agence]
    nombrePostes: t.Optional[int]
    accessibleTH: t.Optional[bool]
    deplacementCode: t.Optional[str]
    deplacementLibelle: t.Optional[str]
    qualificationCode: t.Optional[QualificationCode]
    qualificationLibelle: t.Optional[QualificationLibelle]
    secteurActivite: t.Optional[str] = Field(
        description=(
            "NAF codes for sectors of activity. It is possible to specify two NAF codes"
            " by separating them with a comma in the character string."
            "Example : 01,02"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}secteursActivites"
        ),
    )
    secteurActiviteLibelle: t.Optional[str] = Field(
        description=(
            "Sector of activitylabel"
            "A GET request for the list of accepted choices from the Offres"
            " d'emploi API"
            "to this endpoint :"
            f" {POLE_EMPLOI_REFERENCES_ENDPOINT}secteursActivites"
        ),
    )
    qualitesProfessionnelles: t.Optional[List[QualitePro]]

    # validators
    _validate_dateCreation = validator("dateCreation", allow_reuse=True)(validate_date)
    _validate_dateActualisation = validator("dateActualisation", allow_reuse=True)(
        validate_date
    )
