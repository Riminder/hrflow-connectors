import datetime
import decimal

from msgspec import Struct


class AdmenMission(Struct):
    ID_MISSION: int
    ID_SOCIETE: int
    ID_CONSULTANT: int
    REF_INTERNE: str
    LIBELLE: str
    NB_RECRUT: int
    DESC_MISSION: str
    DATE_PROPOSITION: datetime.date
    MONTANT: decimal.Decimal
    ETAT: int
    TYPE_MISSION: int
    DATE_ACCORD_CLIENT: datetime.date
    DATE_PRESENTATION: datetime.date
    DATE_SIGNATURE: datetime.date
    DATE_ENTREE_FONCT: datetime.date
    SALAIRE_ENTREE: decimal.Decimal
    COMMENTAIRE: str
    FRAIS: float
    REGLE_FACT: int
    MONNAIE_FACT: str
    ANGLAIS: int
    CONDITION_REGL: str
    ABOUTIE: int
    LIBRE1: str
    LIBRE2: str
    LIBRE3: str
    LIBRE4: str
    ID_CONSULTANT_2: int
    FACT1: decimal.Decimal
    FACT2: decimal.Decimal
    FACT3: decimal.Decimal
    FACT4: decimal.Decimal
    FACT5: decimal.Decimal
    FACT6: decimal.Decimal
    DATE_FACT1: datetime.date
    DATE_FACT2: datetime.date
    DATE_FACT3: datetime.date
    DATE_FACT4: datetime.date
    DATE_FACT5: datetime.date
    DATE_FACT6: datetime.date
    ID_PERS1: int
    ID_PERS2: int
    ID_PERS3: int
    ID_PERS4: int
    SALAIRE_DEMANDE: decimal.Decimal
    SECTEUR: str
    FONCTION: str
    REMPLACEMENT: int
    DATE_DEB_RECHERCHE: datetime.date
    INTERROMPUE: int
    DATE_INTERROMPUE: datetime.date
    COMMENT_INTERROMPUE: str
    REPRISE: int
    DATE_REPRISE: datetime.date
    COMMENT_REPRISE: str
    PUBLIEE: int
    TYPECONTRAT: str
    REGION: str
    PAYS: str
    CITY: str
    SECTEURWEB: str
    FONCTIONWEB: str
    HTML: str
    SOFT_DELETED: int
    DATE_DELETED: datetime.date
    ID_PERDEL: int
    DATE_SAISIE: datetime.date
    COLOR: int
    LIST1: str
    LIST2: str
    MONTANT_HONORAIRES: float
    MONTANT_FRAIS: float
    NUMORDER: str
    PROCESS: str
    CL1: str
    CL2: str
    ID_CREATOR: int
    CCCREATOR: str
    CTCREATOR: str
    COMBIN: bytes
    COMBGCOLOR: float
    SALARYMIN: decimal.Decimal
    SALARYMAX: decimal.Decimal
    PERIOD: str
    ALLOTMENT: decimal.Decimal
    ANONYMOUS: int
    ID_CONSULTANT_3: int
    ZIP: str
    EDUCLEVEL: str
    JOBEXP: str
    TYPEJOB: str
    INVOICECONTACT: str
    ID_INVOICECONTACT: int
    SALARYGROUP: str
    CREATEDFACT1: int
    CREATEDFACT2: int
    CREATEDFACT3: int
    NUMFACT1: str
    NUMFACT2: str
    NUMFACT3: str
    PERCENTFACT1: int
    PERCENTFACT2: int
    PERCENTFACT3: int
    CONFIDENTIAL: int
    CONTRACTAMOUNT: decimal.Decimal
    DISTRIBUTIONWITHMARGE: int
    SHOWEMAILASSI: int
    URL: str
    POSTIT: str
    DATE_MAJ: str
    INDEXED: int
    CAC_FILTER: str
    CAC_PERS1_ACCESS: int
    CAC_PERS2_ACCESS: int
    CAC_PERS3_ACCESS: int
    CAC_PERS4_ACCESS: int
    CAC_ACCESS: int
    CAC_SHOW_DOC: str
    EASYRECRUECAMPAIGNID: int
    ASSESSFIRSTID: int
    ID_ISO_CURRENCY: str
    ID_DOCREF: int
    DESC_CUSTOMER: str
    ENTITE: str


class AdmenJob(Struct):
    ID_ANNONCE: int
    ID_SUPPORT: int
    ID_MISSION: int
    REFERENCE: str
    LIBELLE: str
    TEXTE_ANNONCE: str
    ETAT_ANNONCE: int
    COUT: float
    IMPUTABLE: int
    DATE_DEBUT: str
    DATE_FIN: str
    HTML: str
    SOFT_DELETED: int
    DATE_DELETED: str
    ID_PERDEL: int
    TARIF: str
    TYPE: str
    AGENCE: int
    FACTURE: str
    COLOR: int
    DESCRCUSTOMER: str
    DESCRASSIGNMENT: str
    ID_CREATOR: int
    CCCREATOR: str
    CTCREATOR: str
    VALIDATED: int
    METADESCRIPTION: str
    METAKEYS: str
    COMMENTAIRES: str
    DATE_CREATION: str
    ID_MULTIPOSTER: int
    STATUSMULTIPOSTER: int
    URL: str
    TEXTE_ANNONCE_TXT: str
    HTML_TXT: str
    DESCRCUSTOMER_TXT: str
    DESCRASSIGNMENT_TXT: str


class AdmenCandidature(Struct):
    ID_MISSION: int
    ID_PERSONNE: int
    PRESELECTIONNE: int
    SELECTIONNE: int
    CHOISI: int
    ENTRETIEN: int
    DATE_ENTRETIEN: str
    FIRST_ENTRETIEN: int
    DATE_1ER_ENTRETIEN: str
    CR_ENTRETIEN: str
    CR_RDV_1: str
    QUALIFIE: int
    ANNONCE: int
    EVALUATION: int
    SECOND_ENTRETIEN: int
    THIRD_ENTRETIEN: int
    OFFRE: int
    CR_RDV_2: str
    CR_RDV_3: str
    ACCUSE_RECEPT: int
    DATE_RDV_2: str
    DATE_RDV_3: str
    ID_ANNONCE: int
    DATE_AR: str
    INTERNE_ENTRETIEN: str
    EXTERNE_ENTRETIEN: str
    INTERNE_RDV1: str
    EXTERNE_RDV1: str
    INTERNE_RDV2: str
    EXTERNE_RDV2: str
    INTERNE_RDV3: str
    EXTERNE_RDV3: str
    GRAPHOLOGIE: int
    REPONSE_NEGATIVE: int
    date_reponse_neg: str
    NON_INTERESSE: int
    DESISTEMENT: int
    TYPE_DE_CONTRAT: str
    CAND_SAL_FIXE: int
    CAND_SAL_VARIABLE: int
    CAND_SALAIRE: int
    CAND_SAL_ANNEE: int
    CAND_STOCK_OPTION: int
    CAND_SAL_GROUPE: str
    CAND_SAL_AVANTAGE: str
    ORIGINE: str
    DUREE: str
    NON_INTERESSANT: int
    COLOR: str
    TYPE: int
    ID_ADVERTLINES: int
    ID_PERSONNE_WEB: int
    CHECK_C16: int
    CHECK_C17: int
    CHECK_C18: int
    CHECK_C19: int
    CHECK_C20: int
    CHECK_C21: int
    CHECK_C22: int
    CHECK_C23: int
    CHECK_C24: int
    CHECK_C25: int
    DATE_C1: str
    DATE_C2: str
    DATE_C3: str
    DATE_C4: str
    DATE_C5: str
    DATE_C6: str
    DATE_C7: str
    DATE_C8: str
    DATE_C9: str
    DATE_C10: str
    DATE_C11: str
    DATE_C12: str
    DATE_C13: str
    DATE_C14: str
    DATE_C15: str
    DATE_C16: str
    DATE_C17: str
    DATE_C18: str
    DATE_C19: str
    DATE_C20: str
    DATE_C21: str
    DATE_C22: str
    DATE_C23: str
    DATE_C24: str
    DATE_C25: str
    ID_ENVOI: int
    COMMENTS: str
    DATETOPERSON: str
    EXPORTED: int
    DATEEXPORT: str
    NAME: str
    FIRSTNAME: str
    MOBILEPHONE: str
    ID_CANDIDATURE: int
    ID_EASYRECRUECAND: int
    CUSTOMER_FEEDBACK: int
    APPLICATIONCOLOR: int
    Libre1: str


class AdmenProfessionalExperience(Struct):
    ID_PERSONNE: int
    DATE_EXP: datetime.date
    RAISON_SOCIALE: str
    NB_SUPLEANTS: int
    COMMENTAIRES: str
    TECHNIQUE: int
    POSTE_OCCUPE: str
    DATE_FIN: datetime.date
    SECTEUR: str
    EN_COURS: int
    CA_GERE: int
    ID_SOCIETE: int
    INTITULE_POSTE: str
    T_ID_ORGANIGRAMME: int
    REMUNERATION: int
    CURRENCY: str
    ID_BOSS: int
    ID_PERSONNE_WEB: int
    SECONDARYPOSTE: int
    DEPARTMENT: str
    ID_EXPERIENCE: int
    ID_COMPADDR: int


class AdmenProfile(Struct):
    ID_PERSONNE: int
    PER_ID_PERSONNE: int
    ID_SOCIETE: int
    CIVILITE: str
    NOM: str
    PRENOM: str
    ADRESSE: str
    CODE_POSTAL: str
    VILLE: str
    TEL_DIRECT: str
    TEL_STANDARD: str
    TEL_PERSO: str
    TEL_MOBILE: str
    EMAIL: str
    PAGER: str
    FAX_BUREAU: str
    FAX_PERSO: str
    POSTE_OCCUPE: str
    DATE_NAISSANCE: datetime.date
    CHASSE: int
    PROFIL_CANDIDAT: int
    DEJA_CANDIDAT: int
    DEJA_RENCONTRE: int
    CV: int
    DATE_CV: datetime.date
    MOBILITE: int
    EMAIL_PERSO: str
    MOBILITE_TXT: str
    ID_CONSULTANT: int
    GROUPE_SALAIRE: str
    SALAIRE_FIXE: int
    SALAIRE_VARIABLE: int
    SALAIRE_AVANTAGE: str
    ENCADREMENT: int
    NOMBRE_PERSONNES: int
    COMMENTAIRE: str
    PROSPECT: int
    CLIENT: int
    CANDIDAT: int
    TYPOLOGIE: int
    STOCK_OPTION: int
    EMAIL_DEFAUT: int
    MOBILE_DEFAULT: int
    SITU_FAMILLE: str
    LIBRE1: str
    LIBRE2: str
    LIBRE3: str
    LIBRE4: str
    TEL1_CONFIDENTIEL: int
    TEL2_CONFIDENTIEL: int
    TEL3_CONFIDENTIEL: int
    TEL4_CONFIDENTIEL: int
    GRAPHOLOGIE: int
    DATE_CREATION: datetime.date
    DATE_MAJ: datetime.datetime
    DATE_DEPUIS: datetime.date
    NATIONALITE: str
    SALAIRE: int
    LIB_SOCIETE: str
    NOM_SECRETAIRE: str
    PRENOM_SECRETAIRE: str
    TEL_SECRETAIRE: str
    REFERENCE: int
    INTITULE_POSTE: str
    POSTE_RECHERCHE: str
    INTERNET: int
    SPONTANEE: int
    PAYS: str
    VAL_NOTE1: int
    VAL_NOTE2: int
    VAL_NOTE3: int
    VAL_NOTE4: int
    FOURNISSEUR: int
    PRESCRIPTEUR: int
    PARTENAIRE: int
    ACTIF: int
    DOSSIER: int
    OFFLIMIT_AUTO: int
    OFFLIMIT_MANU: int
    PUBLIE: int
    ANNEE_SALAIRE: int
    ORIGINE: str
    DATE_MAJ_MANU: datetime.datetime
    TEL_PERSO2: str
    TEL_PERSO3: str
    MOBILE_PRO: str
    EN_COURS: int
    SOFT_DELETED: int
    DATE_DELETED: datetime.date
    ID_PERDEL: int
    COLOR: int
    LASTACTION: str
    CL1: str
    CL2: str
    CL3: str
    CS1: str
    CS2: str
    LIST1: str
    LIST2: str
    LIST3: str
    LIST4: str
    AREA: str
    CURRENCY: str
    MASK11: int
    MASK12: str
    MASK13: str
    MASK14: str
    MASK15: str
    COMPNAME: str
    ID_CREATOR: int
    CCCREATOR: str
    CTCREATOR: str
    SECTEUR: str
    COMBIN: str
    COMBGCOLOR: float
    ID_PERSONNE_WEB: int
    LASTDATEACTION: datetime.date
    LASTLIBACTION: str
    VALIDATED: int
    ASSISTANTMAIL: str
    LASTUSERACTION: str
    IMPORTED: int
    ID_LINKEDIN: str
    LINKEDINPUBLICPROFIL: str
    scsync_uri_viadeo: str
    scsync_uri_xing: str
    scsync_modified: str
    ID_AWA2: int
    POSTIT: str
    ID_VIADEO: str
    VIADEOPUBLICPROFIL: str
    INDEXED: int
    CAC_USER: str
    CAC_PWD: str
    CAC_DATE_END: datetime.date
    EXPECTEDSALARYMIN: int
    EXPECTEDSALARYMAX: int
    EMAILINGCONTACT: int
    SKYPELOGIN: str
    TWITTERPAGE: str
    FACEBOOKPAGE: str
    ID_COMPADDR: int
    ID_DOCREF: int
    GENDER: str
    ID_ISO_CURRENCY: str
    RGPD_VALIDATED: int
    RGPD_VALIDATEDDATE: datetime.date
    RGPD_ASKEDDATE: datetime.date
    RGPD_DISABLED: int
    Libre5: str
    Libre6: str
    Libre7: str
    Libre8: str
    ID_CONTACT: int
