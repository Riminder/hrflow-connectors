import datetime
import typing as t
from enum import Enum, IntEnum

from msgspec import Meta, Struct
from typing_extensions import Annotated

POLE_EMPLOI_REFERENCES_ENDPOINT = (
    "https://api.emploi-store.fr/partenaire/offresdemploi/v2/referentiel/"
)


def validate_date(value: t.Optional[str], field_name: str) -> None:
    if value is not None:
        try:
            datetime.date.fromisoformat(value)
        except ValueError:
            raise ValueError(
                f"Invalid date format for '{field_name}': {value}. Expected ISO 8601"
                " format (YYYY-MM-DD)."
            )


class JobLocation(Struct):
    libelle: str
    latitude: float
    longitude: float
    codepostal: str
    commune: Annotated[
        str,
        Meta(
            description=(
                "INSEE code of the commune"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}communes"
            ),
        ),
    ]


class Entreprise(Struct):
    nom: t.Optional[str]
    description: t.Optional[str]
    logo: t.Optional[str]
    url: t.Optional[str]
    entrepriseAdaptee: bool


class Partner(Struct):
    nom: str
    url: str
    logo: str


class OfferOriginTag(IntEnum):
    POLE_EMPLOI = 1
    PARTNER = 2


class OfferOrigin(Struct):
    origine: OfferOriginTag
    urlOrigine: t.Optional[str]
    partenaires: t.List[Partner]


class ExperienceRequirement(str, Enum):
    BEGINNER_ACCEPTED = "D"
    EXPERIENCE_DESIRED = "S"
    EXPERIENCE_REQUIRED = "E"


class Exigence(Enum):
    REQUIRED = "E"
    DESIRED = "S"


class Formation(Struct):
    domaineLibelle: str
    niveauLibelle: Annotated[
        str,
        Meta(
            description=(
                "Label of the level of the education required"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}referentiel/niveauxFormations"
            ),
        ),
    ]
    commentaire: str
    exigence: Exigence


class Langue(Struct):
    libelle: Annotated[
        str,
        Meta(
            description=(
                "Language label"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}referentiel/langues"
            ),
        ),
    ]
    exigence: t.Optional[Exigence]


class Permis(Struct):
    libelle: Annotated[
        str,
        Meta(
            description=(
                "requested license"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}referentiel/permis"
            ),
        ),
    ]
    exigence: t.Optional[Exigence]


class Competence(Struct):
    code: str
    libelle: str
    exigence: t.Optional[Exigence]


class Salaire(Struct):
    libelle: t.Optional[str]
    commentaire: t.Optional[str]
    complement1: t.Optional[str]
    complement2: t.Optional[str]


class Contact(Struct):
    nom: t.Optional[str]
    coordonnees1: t.Optional[str]
    coordonnees2: t.Optional[str]
    coordonnees3: t.Optional[str]
    telephone: t.Optional[str]
    courriel: t.Optional[str]
    commentaire: t.Optional[str]
    urlRecruteur: t.Optional[str]
    urlPostulation: t.Optional[str]


class Agence(Struct):
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


class QualitePro(Struct):
    libelle: t.Optional[str]
    description: t.Optional[str]


class FranceTravailJobOffer(Struct):
    id: int
    intitule: str
    description: str
    dateCreation: t.Optional[str] = None
    dateActualisation: t.Optional[str] = None
    lieuTravail: t.Optional[JobLocation] = None
    romeCode: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "ROME code of the profession"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}metiers"
            ),
        ),
    ] = None
    romeLibelle: t.Optional[str] = None
    appellationLibelle: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Code of the appellation"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}appellations"
            ),
        ),
    ] = None
    entreprise: t.Optional[Entreprise] = None
    typeContrat: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Contract type code"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}typesContrats"
            ),
        ),
    ] = None
    typeContratLibelle: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Contract type label"
                "Example : CDI,CDD"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}typesContrats"
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
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}naturesContrats"
            ),
        ),
    ] = None
    origineOffre: t.Optional[OfferOrigin] = None
    offresManqueCandidats: t.Optional[bool] = None
    experienceExige: t.Optional[ExperienceRequirement] = None
    experienceLibelle: t.Optional[str] = None
    experienceCommentaire: t.Optional[str] = None
    formations: t.Optional[t.List[Formation]] = None
    langues: t.Optional[t.List[Langue]] = None
    permis: t.Optional[t.List[Permis]] = None
    outilsBureautiques: t.Optional[str] = None
    competences: t.Optional[t.List[Competence]] = None
    salaire: t.Optional[Salaire] = None
    dureeTravailLibelle: t.Optional[str] = None
    dureeTravailLibelleConverti: t.Optional[str] = None
    complementExercice: t.Optional[str] = None
    conditionExercice: t.Optional[str] = None
    alternance: t.Optional[bool] = None
    contact: t.Optional[Contact] = None
    agence: t.Optional[Agence] = None
    nombrePostes: t.Optional[int] = None
    accessibleTH: t.Optional[bool] = None
    deplacementCode: t.Optional[str] = None
    deplacementLibelle: t.Optional[str] = None
    qualificationCode: t.Optional[QualificationCode] = None
    qualificationLibelle: t.Optional[QualificationLibelle] = None
    secteurActivite: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "NAF codes for sectors of activity. It is possible to specify two NAF"
                " codes by separating them with a comma in the character"
                " string.Example : 01,02A GET request for the list of accepted choices"
                " from the Offres d'emploi APIto this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}secteursActivites"
            ),
        ),
    ] = None
    secteurActiviteLibelle: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Sector of activitylabel"
                "A GET request for the list of accepted choices from the Offres"
                " d'emploi API"
                "to this endpoint :"
                f" {POLE_EMPLOI_REFERENCES_ENDPOINT}secteursActivites"
            ),
        ),
    ] = None
    qualitesProfessionnelles: t.Optional[t.List[QualitePro]] = None

    def __post_init__(self):
        # Validate date fields
        validate_date(self.dateCreation, "dateCreation")
        validate_date(self.dateActualisation, "dateActualisation")
