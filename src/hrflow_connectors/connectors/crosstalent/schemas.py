from pydantic import BaseModel, Field
from typing import Optional, List, Dict, Any


class CrosstalentJob(BaseModel):
    Name: str = Field("Undefined")
    Id: str = Field(..., description="ID d'enregistrement")
    createdDate: Optional[str]
    crtarecr__URL_of_the_form_on_job_offer__c: Optional[str] = Field(
        None, description="URL of the job"
    )
    crta__Location__Latitude__s: Optional[float] = Field(
        None, description="Geolocation latitude"
    )
    crta__Location__Longitude__s: Optional[float] = Field(
        None, description="Geolocation longitude"
    )
    Lieu__c: Optional[str] = Field(None, description="Address")
    crta__CT_City__c: Optional[str] = Field(None, description="City")
    crta__CT_Country__c: Optional[str] = Field(None, description="Country")
    crta__CT_Postal_code__c: Optional[str] = Field(None, description="Postal code")
    crta__Postal_Code__c: Optional[str] = Field(None, description="Postal code")
    crta__CT_Description__c: Optional[str] = Field(None, description="Job description")
    crta__CT_Benefices_attendus__c: Optional[str] = Field(
        None, description="Job benefits"
    )
    crta__CT_Description__c: Optional[str] = Field(None, description="Job description")
    crta__CT_Detail_motif_recours_CDD__c: Optional[str]
    crta__CT_Horaires_particuliers__c: Optional[str]
    crta__CT_Needed_skills__c: Optional[str]
    crta__CT_Other_contract_benefits__c: Optional[str]
    crta__CT_Other_salary_benefits__c: Optional[str]
    crta__CT_Remuneration_variable__c: Optional[str]
    crta__RC_Service__c: Optional[str]
    crtarecr__Personalised_header__c: Optional[str]
    crtarecr__Required_Profile__c: Optional[str]
    Profil_recherche__c: Optional[str]
    crtarecr__Language_1__c: Optional[str]
    crtarecr__Language_level_1__c: Optional[str]
    crtarecr__Language_2__c: Optional[str]
    crtarecr__Language_level_2__c: Optional[str]
    crtarecr__Language_3__c: Optional[str]
    crtarecr__Language_level_3__c: Optional[str]
    OwnerId: Optional[str]
    IsDeleted: Optional[bool]
    CurrencyIsoCode: Optional[str]
    RecordTypeId: Optional[str]
    CreatedById: Optional[str]
    LastModifiedDate: Optional[str]
    LastModifiedById: Optional[str]
    SystemModstamp: Optional[str]
    LastActivityDate: Optional[str]
    LastViewedDate: Optional[str]
    LastReferencedDate: Optional[str]
    crta__Etat__c: Optional[str]
    crta__CT_Avec_RTT__c: Optional[bool]
    crta__CT_Annee_budget__c: Optional[str]
    crta__CT_Contact_Person__c: Optional[str]
    crta__CT_Motif_recours_CDD__c: Optional[str]
    crta__CT_Nom_demandeur_DAR__c: Optional[str]
    crta__Contrainte__c: Optional[str]
    crta__Categorie__c: Optional[str]
    crta__CT_Organization_name__c: Optional[str]
    crta__Diffusion_de_l_offre__c: Optional[bool]
    crta__Origine_de_l_offre__c: Optional[str]
    crta__Email_du_recruteur__c: Optional[str]
    crta__Temps_de_travail_en_pourcentage__c: Optional[str]
    crta__Filiere__c: Optional[str]
    crta__CT_Date__c: Optional[str]
    crta__Contrat__c: Optional[str]
    crta__CT_Salary__c: Optional[str]
    crta__Date_max_du_positionnement__c: Optional[str]
    crta__CT_Nom_Responsable_hierarchique__c: Optional[str]
    crta__CT_Duration__c: Optional[str]
    crta__CT_Duree_Stage__c: Optional[str]
    crta__CT_Email_Manager__c: Optional[str]
    crta__CT_End_date__c: Optional[str]
    crta__CT_Modalite_attribution_remun_variable__c: Optional[str]
    crta__CT_Industry_of_activities__c: Optional[str]
    crta__CT_Date_debut_Contrat__c: Optional[str]
    crta__CT_Date_fin_Contrat__c: Optional[str]
    crta__CT_date_validation_DAR__c: Optional[str]
    crta__CT_Motif_CDI__c: Optional[str]
    crta__CT_Number_of_opened_positions__c: Optional[int]
    crta__CT_Prevu_au_budget__c: Optional[bool]
    crta__CT_Publiee_Site_web_externe__c: Optional[bool]
    crta__CT_Scope__c: Optional[bool]
    crta__CT_Entrprise_Postal_code__c: Optional[str]
    crta__Date_limite_de_reponse__c: Optional[str]
    crta__Duree_ouverture_mois__c: Optional[int]
    crta__Etat__c: Optional[str]
    crta__Status__c: Optional[str]
    crtarecr__Archived__c: Optional[bool]
    crtarecr__Contract_type__c: Optional[str]
    crtarecr__Published_on_Jobboards__c: Optional[bool]
    crtarecr__Published_on_Website__c: Optional[bool]
    crtarecr__Recruitment_Request_Date__c: Optional[str]
    crtarecr__Recruitment_request_status__c: Optional[str]
    M_tier__c: Optional[str]
    Famille_de_m_tier__c: Optional[str]
    Sourcing_Auto__c: Optional[bool]
    Disponibilit_imm_diate__c: Optional[bool]
    Date_de_d_but__c: Optional[str]
    Fourchette_de_salaire__c: Optional[str]
    Mobilit_R_gion__c: Optional[str]
    Entit_juridique__c: Optional[str]
    Compte_de_rattachement__c: Optional[str]
    Site_de_diffusion_de_l_offre__c: Optional[str]
    Mobilit_Pays__c: Optional[str]
    Position__c: Optional[str]
    Mobilit_R_gion__c: Optional[str]
    Pays__c: Optional[str]
    Mots_Clefs__c: Optional[str]
    Emploi_Type__c: Optional[str]
    Niveau_d_exp_rience_attendu__c: Optional[str]
    Domaine_d_activite__c: Optional[str]
    Secteur_d_activite__c: Optional[str]
    Date_de_fin__c: Optional[str]
    crtarecr__BU1__c: Optional[str]
    crtarecr__BU2__c: Optional[str]
    crtarecr__Contractual_status__c: Optional[str]
    crtarecr__Employment_start_date__c: Optional[str]
    crtarecr__End_date_of_Intranet_Site_publication__c: Optional[str]
    crtarecr__End_date_of_Jobboard_publication__c: Optional[str]
    crtarecr__End_date_of_Website_publication__c: Optional[str]
    crtarecr__Envisaged_annual_gross_remuneration__c: Optional[str]
    crtarecr__HR_Function__c: Optional[str]
    crtarecr__Max_salary__c: Optional[int]
    crtarecr__Max_term_months__c: Optional[int]
    crtarecr__Min_salary__c: Optional[int]
    crtarecr__Min_term_months__c: Optional[int]
    crtarecr__Period__c: Optional[str]
    crtarecr__Recruiter_Firstname__c: Optional[str]
    crtarecr__Recruiter_Lastname__c: Optional[str]
    crtarecr__Reference__c: Optional[str]
    crtarecr__Required_Experience_Level__c: Optional[str]
    crtarecr__Required_diploma_level__c: Optional[str]
    crtarecr__Start_date_of_Intranet_Site_publication__c: Optional[str]
    crtarecr__Start_date_of_Jobboard_publication__c: Optional[str]
    crtarecr__Start_date_of_Website_publication__c: Optional[str]
    crtarecr__Type_of_employment__c: Optional[str]
    crtarecr__Weekly_Working_Time_in_hours__c: Optional[int]
    crta__CT_Designation__c: Optional[str]
    crta__Location__c: Optional[dict]
    crta__Nb_positionnements__c: Optional[int]
    crta__Texte_statut__c: Optional[str]
    crta__Valeur_statut__c: Optional[int]
    crtarecr__Link_of_the_form_on_job_offer__c: Optional[str]
    crtarecr__Nb_of_applications_still_to_be_processed__c: Optional[int]
    Besoin_client__c: Optional[str]
    crta__Entite__c: Optional[str]


class FieldLocation(BaseModel):
    """ Location for profile info, experience and education information"""

    text: Optional[str] = Field(None, description="Location text address.")
    lat: Optional[float] = Field(
        None, description="Geocentric latitude of the Location."
    )
    lng: Optional[float] = Field(
        None, description="Geocentric longitude of the Location."
    )
    Fields: Optional[Dict[str, Any]] = Field(
        None, description="other location attributes like country, country_codeNoneetc"
    )


class FieldSkill(BaseModel):
    name: str = Field(..., description="Identification name of the skill")
    type: str = Field(..., description="Type of the skill. hard or soft")
    value: Optional[str] = Field(None, description="Value associated to the skill")


class InfoUrls(BaseModel):
    """ Porfile urls """

    from_resume: Optional[List[str]]
    linkedin: Optional[str]
    twitter: Optional[str]
    facebook: Optional[str]
    github: Optional[str]


class HrflowProfileField(BaseModel):
    """
    HrflowJobField: Languages, Certifications, Courses, Tasks, Interests, Metadatas, Tags
    are arrays of objects following the JSON structure below
    """

    name: str = Field(..., description="Identification name of the Object")
    value: Optional[str] = Field(
        None, description="Value associated to the Object's name"
    )


class CrosstalentProfileInfo(BaseModel):
    """ Profile basic informations"""

    full_name: Optional[str]
    first_name: Optional[str]
    last_name: str
    email: str
    phone: Optional[str]
    date_birth: Optional[str] = Field(None, description="Profile date of birth")
    location: Optional[FieldLocation] = Field(
        None, description="Profile location object"
    )
    urls: Optional[InfoUrls] = Field(
        None, description="Profile social networks and URLs"
    )
    picture: Optional[str] = Field(None, description="Profile picture url")
    gender: Optional[str] = Field(None, description="Profile gender")
    summary: Optional[str] = Field(None, description="Profile summary text")


class Experience(BaseModel):
    key: Optional[str] = Field(
        None, description="Identification key of the Experience."
    )
    company: Optional[str] = Field(None, description="Company name of the Experience.")
    title: Optional[str] = Field(None, description="Title of the Experience.")
    description: Optional[str] = Field(
        None, description="Description of the Experience."
    )
    location: Optional[FieldLocation] = Field(
        None, description="Location object of the Experience."
    )
    date_start: Optional[str] = Field(
        None, description="Start date of the experience. type: ('datetime ISO 8601')"
    )
    date_end: Optional[str] = Field(
        None, description="End date of the experience. type: ('datetime ISO 8601')"
    )
    skills: Optional[List[FieldSkill]] = Field(
        None, description="List of skills of the Experience."
    )
    certifications: Optional[List[HrflowProfileField]]
    courses: Optional[List[HrflowProfileField]]
    tasks: Optional[List[HrflowProfileField]]


class Education(BaseModel):
    key: Optional[str] = Field(None, description="Identification key of the Education.")
    school: Optional[str] = Field(None, description="School name of the Education.")
    title: Optional[str] = Field(None, description="Title of the Education.")
    description: Optional[str] = Field(
        None, description="Description of the Education."
    )
    location: Optional[FieldLocation] = Field(
        None, description="Location object of the Education."
    )
    date_start: Optional[str] = Field(
        None, description="Start date of the Education. type: ('datetime ISO 8601')"
    )
    date_end: Optional[str] = Field(
        None, description="End date of the Education. type: ('datetime ISO 8601')"
    )
    skills: Optional[List[FieldSkill]] = Field(
        None, description="List of skills of the Education."
    )
    certifications: Optional[List[HrflowProfileField]]
    courses: Optional[List[HrflowProfileField]]
    tasks: Optional[List[HrflowProfileField]]


class CrosstalentProfile(BaseModel):
    """ Crosstalent Push Profile object model"""

    key: Optional[str] = Field(None, description="Identification key of the Profile.")
    reference: Optional[str] = Field(
        None, description="Custom identifier of the Profile."
    )
    archieved_at: Optional[str] = Field(
        None,
        description="type: datetime ISO8601, Archive date of the Profile. The value is null for unarchived Profiles.",
    )
    updated_at: Optional[str] = Field(
        None, description="type: datetime ISO8601, Last update date of the Profile."
    )
    created_at: Optional[str] = Field(
        None, description="type: datetime ISO8601, Creation date of the Profile."
    )
    info: CrosstalentProfileInfo = Field(
        ..., description="Object containing the Profile's info."
    )
    text_language: str = Field(
        ..., description="Code language of the Profile. type: string code ISO 639-1"
    )
    text: str = Field(..., description="Full text of the Profile..")
    experiences_duration: float = Field(
        ..., description="Total number of years of experience."
    )
    educations_duration: float = Field(
        ..., description="Total number of years of education."
    )
    experiences: Optional[List[Experience]] = Field(
        None, description="List of experiences of the Profile."
    )
    educations: Optional[List[Education]] = Field(
        None, description="List of educations of the Profile."
    )
    attachments: List = Field(
        ..., description="List of documents attached to the Profile."
    )
    skills: Optional[List[FieldSkill]] = Field(
        None, description="List of skills of the Profile."
    )
    languages: Optional[List[HrflowProfileField]] = Field(
        None, description="List of spoken languages of the profile"
    )
    certifications: Optional[List[HrflowProfileField]] = Field(
        None, description="List of certifications of the Profile."
    )
    courses: Optional[List[HrflowProfileField]] = Field(
        None, description="List of courses of the Profile."
    )
    tasks: Optional[List[HrflowProfileField]] = Field(
        None, description="List of tasks of the Profile."
    )
    interests: Optional[List[HrflowProfileField]] = Field(
        None, description="List of interests of the Profile."
    )
    labels: Optional[List[HrflowProfileField]] = Field(
        None, description="List of labels of the Profile."
    )
    tags: Optional[List[HrflowProfileField]] = Field(
        None, description="List of tags of the Profile."
    )
    metadatas: Optional[List[HrflowProfileField]] = Field(
        None, description="List of metadatas of the Profile."
    )
