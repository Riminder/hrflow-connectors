/**
 * Poleemploi Schemas
 * Complete TypeScript translation of schemas.py
 */

export interface JobLocation {
  libelle: string;
  latitude: number;
  longitude: number;
  codepostal: string;
  commune: string;
}

export interface Entreprise {
  nom: str | undefined;
  description: str | undefined;
  logo: str | undefined;
  url: str | undefined;
  entrepriseAdaptee: boolean;
}

export interface Partner {
  nom: string;
  url: string;
  logo: string;
}

export interface OfferOrigin {
  origine: OfferOriginTag;
  urlOrigine: str | undefined;
  partenaires: Partner[;
}

export interface Formation {
  domaineLibelle: string;
  niveauLibelle: string;
  commentaire: string;
  exigence: Exigence;
}

export interface Langue {
  libelle: string;
  exigence: Exigence | undefined;
}

export interface Permis {
  libelle: string;
  exigence: Exigence | undefined;
}

export interface Competence {
  code: string;
  libelle: string;
  exigence: Exigence | undefined;
}

export interface Salaire {
  libelle: str | undefined;
  commentaire: str | undefined;
  complement1: str | undefined;
  complement2: str | undefined;
}

export interface Contact {
  nom: str | undefined;
  coordonnees1: str | undefined;
  coordonnees2: str | undefined;
  coordonnees3: str | undefined;
  telephone: str | undefined;
  courriel: str | undefined;
  commentaire: str | undefined;
  urlRecruteur: str | undefined;
  urlPostulation: str | undefined;
}

export interface Agence {
  telephone: str | undefined;
  courriel: str | undefined;
}

export interface QualitePro {
  libelle: str | undefined;
  description: str | undefined;
}

export interface PoleEmploiJobOffer {
  id: number;
  intitule: string;
  description: string;
  dateCreation: str | undefined;
  dateActualisation: str | undefined;
  lieuTravail: JobLocation | undefined;
  romeCode: str | undefined;
  romeLibelle: str | undefined;
  appellationLibelle: str | undefined;
  entreprise: Entreprise | undefined;
  typeContrat: str | undefined;
  typeContratLibelle: str | undefined;
  natureContrat: str | undefined;
  origineOffre: OfferOrigin | undefined;
  offresManqueCandidats: bool | undefined;
  experienceExige: ExperienceRequirement | undefined;
  experienceLibelle: str | undefined;
  experienceCommentaire: str | undefined;
  formations: List[Formation | undefined;
  langues: List[Langue | undefined;
  permis: List[Permis | undefined;
  outilsBureautiques: str | undefined;
  competences: List[Competence | undefined;
  salaire: Salaire | undefined;
  dureeTravailLibelle: str | undefined;
  dureeTravailLibelleConverti: str | undefined;
  complementExercice: str | undefined;
  conditionExercice: str | undefined;
  alternance: bool | undefined;
  contact: Contact | undefined;
  agence: Agence | undefined;
  nombrePostes: int | undefined;
  accessibleTH: bool | undefined;
  deplacementCode: str | undefined;
  deplacementLibelle: str | undefined;
  qualificationCode: QualificationCode | undefined;
  qualificationLibelle: QualificationLibelle | undefined;
  secteurActivite: str | undefined;
  secteurActiviteLibelle: str | undefined;
  qualitesProfessionnelles: List[QualitePro | undefined;
}

