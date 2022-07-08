import datetime
import typing as t
from enum import Enum, IntEnum
from typing import Any, List

from pydantic import BaseModel, validator

from hrflow_connectors.connectors.poleemploi.referentials import (
    Listofappelations,
    ListofcodeROME,
    Listofcommunes,
    Listoflanguages,
    ListofnatureContrats,
    ListofniveauFormations,
    ListofpermisLibelles,
    ListofsecteursActivite,
    ListofsecteursActiviteLibelles,
    ListoftypesContratLibelles,
    ListoftypesContrats,
)


def validate_date(value: Any) -> Any:
    try:
        _ = datetime.date.fromisoformat(value)
        return value
    except ValueError:
        raise ValueError(
            f"Invalid time format given {value}, expected format ISO 8601 format"
            " ISO-8601 (YYYY-MM-DDTHH:MM:SSZ)"
        )


Appellation = Enum("Appellation", dict(Listofappelations), type=str)
CodeROME = Enum("CodeROME", dict(ListofcodeROME), type=str)
Commune = Enum("Commune", dict(Listofcommunes), type=str)
NatureContrat = Enum("NatureContrat", dict(ListofnatureContrats), type=str)
NiveauFormation = Enum("NiveauFormation", dict(ListofniveauFormations), type=str)
SecteurActivite = Enum("SecteurActivite", dict(ListofsecteursActivite), type=str)
TypeContrat = Enum("TypeContrat", dict(ListoftypesContrats), type=str)
LibelleLangue = Enum("LibelleLangue", dict(Listoflanguages), type=str)
LibellePermis = Enum("LibellePermis", dict(ListofpermisLibelles), type=str)
TypeContratLibelle = Enum(
    "TypeContratLibelle", dict(ListoftypesContratLibelles), type=str
)
SecteurActiviteLibelle = Enum(
    "SecteurActiviteLibelle", dict(ListofsecteursActiviteLibelles), type=str
)


class JobLocation(BaseModel):
    libelle: str
    latitude: float
    longitude: float
    codepostal: str
    commune: Commune


class Entreprise(BaseModel):
    nom: t.Optional[str]
    description: t.Optional[str]
    logo: t.Optional[str]
    url: t.Optional[str]
    entrepriseAdaptee: bool


class Partenaire(BaseModel):
    nom: str
    url: str
    logo: str


class OrigineOffreTag(IntEnum):
    poleEmploi = 1
    partenaire = 2


class OrigineOffre(BaseModel):
    origine: OrigineOffreTag
    urlOrigine: t.Optional[str]
    partenaires: List[Partenaire]


class ExperienceExige(str, Enum):
    d = "D"  # Débutant accepté
    s = "S"  # Expérience souhaitée
    e = "E"  # Expérience exigée


class Exigence(Enum):
    e = "E"  # Exigé
    s = "S"  # Souhaité


class Formation(BaseModel):
    domaineLibelle: str
    niveauLibelle: NiveauFormation
    commentaire: str
    exigence: Exigence


class Langue(BaseModel):
    libelle: LibelleLangue
    exigence: t.Optional[Exigence]


class Permis(BaseModel):
    libelle: LibellePermis
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
    a = "1"  # 1 → Manœuvre
    b = "2"  # 2 → Ouvrier spécialisé
    c = "3"  # 3 → Ouvrier qualifié (P1, P2)
    d = "4"  # 4 → Ouvrier qualifié (P3, P4, OHQ)
    e = "5"  # 5 → Employé non qualifié
    f = "6"  # 6 → Employé qualifié
    g = "7"  # 7 → Technicien
    h = "8"  # 8 → Agent de maîtrise
    i = "9"  # 9 → Cadre


class QualificationLibelle(str, Enum):
    a = "Manœuvre"
    b = "Ouvrier spécialisé"
    c = "Ouvrier qualifié (P1, P2)"
    d = "Ouvrier qualifié (P3, P4, OHQ)"
    e = "Employé non qualifié"
    f = "Employé qualifié"
    g = "Technicien"
    h = "Agent de maîtrise"
    i = "Cadre"


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
    romeCode: t.Optional[CodeROME]
    romeLibelle: t.Optional[str]
    appellationLibelle: t.Optional[Appellation]
    entreprise: t.Optional[Entreprise]
    typeContrat: t.Optional[TypeContrat]
    typeContratLibelle: t.Optional[TypeContratLibelle]
    natureContrat: t.Optional[NatureContrat]
    origineOffre: t.Optional[OrigineOffre]
    offresManqueCandidats: t.Optional[bool]
    experienceExige: t.Optional[ExperienceExige]
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
    secteurActivite: t.Optional[SecteurActivite]
    secteurActiviteLibelle: t.Optional[SecteurActiviteLibelle]
    qualitesProfessionnelles: t.Optional[List[QualitePro]]

    # validators
    _validate_dateCreation = validator("dateCreation", allow_reuse=True)(validate_date)
    _validate_dateActualisation = validator("dateActualisation", allow_reuse=True)(
        validate_date
    )
