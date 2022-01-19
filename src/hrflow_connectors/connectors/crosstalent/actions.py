from typing import Iterator, Dict, Any
from pydantic import Field
import requests

from ...core.auth import OAuth2PasswordCredentialsBody

from ...core.action import PullJobsBaseAction, PushProfileBaseAction
from ...utils.hrflow import generate_workflow_response


class PullJobsAction(PullJobsBaseAction):
    auth: OAuth2PasswordCredentialsBody = Field(
        ..., description="Auth instance to identify and communicate with the platform"
    )
    subdomain: str = Field(
        ...,
        description="Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.salesforce.com/ABC`",
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = f"https://{self.subdomain}.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/"
        pull_jobs_request.auth = self.auth
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

        return response.json()

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        job = dict()

        # name
        job["name"] = data.get("Name", "Undefined")

        # reference
        job["reference"] = data.get("Id")

        # created_at
        job["created_at"] = data.get("CreatedDate")

        # url
        job["url"] = data.get("crtarecr__URL_of_the_form_on_job_offer__c")

        # location
        lat = data.get("crta__Location__Latitude__s")
        lng = data.get("crta__Location__Longitude__s")
        text = data.get("Lieu__c")

        geojson = dict()
        geojson["city"] = data.get("crta__CT_City__c")
        geojson["country"] = data.get("crta__CT_Country__c")

        postcode = None
        postcode = data.get("crta__CT_Postal_code__c")
        if postcode is None:
            postcode = data.get("crta__Postal_Code__c")

        geojson["postcode"] = postcode

        job["location"] = dict(lat=lat, lng=lng, text=text, geojson=geojson)

        # summary
        job["summary"] = data.get("crta__CT_Description__c")

        # sections
        job["sections"] = []

        def create_section(field_name: str, title: str = None):
            """
            Create a section in job if `field_name` value is not `None`

            Args:
                field_name (str): Field name in Crosstalent. For example : `crta__RC_Service__c`
                title (str, optional): Section title. Defaults to None.
            """
            section_name = "crosstalent_{}".format(field_name)
            section_title = title
            section_description = data.get(field_name)
            if section_description is not None:
                section = dict(
                    name=section_name,
                    title=section_title,
                    description=section_description,
                )
                job["sections"].append(section)

        ## Add sections
        create_section("crta__CT_Benefices_attendus__c", "Bénéfices attendus")
        create_section("crta__CT_Description__c", "Descriptif du poste")
        create_section(
            "crta__CT_Detail_motif_recours_CDD__c",
            "Précisions si surcroît/motif particulier",
        )
        create_section(
            "crta__CT_Horaires_particuliers__c", "Horaires particuliers à préciser"
        )
        create_section("crta__CT_Needed_skills__c", "Compétences requises")
        create_section(
            "crta__CT_Other_contract_benefits__c", "Autres dispositions contractuelles"
        )
        create_section(
            "crta__CT_Other_salary_benefits__c", "Autres éléments de rémunération"
        )
        create_section("crta__CT_Remuneration_variable__c", "Variable")
        create_section("crta__RC_Service__c", "Service")
        create_section("crtarecr__Personalised_header__c", "Entête personnalisée")
        create_section("crtarecr__Required_Profile__c", "Profil recherché")
        create_section("Profil_recherche__c", "Profil recherché")

        # languages
        job["languages"] = []

        def create_language(field_language_name: str, field_language_level: str):
            """
            Create language in job if `field_language_name` is not `None`

            Args:
                field_language_name (str): Field name in Crosstalent. For example : `crtarecr__Language_1__c`
                field_language_level (str): Field level in Crosstalent. For example : `crtarecr__Language_level_1__c`
            """
            language_name = data.get(field_language_name)
            language_level = data.get(field_language_level)
            if language_name is not None:
                language = dict(name=language_name, value=language_level)
                job["languages"].append(language)

        ## Add languages
        create_language("crtarecr__Language_1__c", "crtarecr__Language_level_1__c")
        create_language("crtarecr__Language_2__c", "crtarecr__Language_level_2__c")
        create_language("crtarecr__Language_3__c", "crtarecr__Language_level_3__c")

        # tags
        job["tags"] = []

        def create_tag(field_name: str):
            """
            Create tag in job if `field_name` is not `None`

            Args:
                field_name (str): Field name in Crosstalent. For example : `crta__CT_Avec_RTT__c`
            """
            tag_name = "crosstalent_{}".format(field_name)
            tag_value = data.get(field_name)
            if tag_value is not None:
                tag = dict(name=tag_name, value=tag_value)
                job["tags"].append(tag)

        ## Add tags
        create_tag("OwnerId")
        create_tag("IsDeleted")
        create_tag("CurrencyIsoCode")
        create_tag("RecordTypeId")
        create_tag("CreatedById")
        create_tag("LastModifiedDate")
        create_tag("LastModifiedById")
        create_tag("SystemModstamp")
        create_tag("LastActivityDate")
        create_tag("LastViewedDate")
        create_tag("LastReferencedDate")
        create_tag("crta__Etat__c")
        create_tag("crta__CT_Annee_budget__c")
        create_tag("crta__CT_Avec_RTT__c")
        create_tag("crta__CT_Contact_Person__c")
        create_tag("crta__CT_Date__c")
        create_tag("crta__CT_Date_debut_Contrat__c")
        create_tag("crta__CT_Date_fin_Contrat__c")
        create_tag("crta__CT_Duration__c")
        create_tag("crta__CT_Duree_Stage__c")
        create_tag("crta__CT_Email_Manager__c")
        create_tag("crta__CT_End_date__c")
        create_tag("crta__CT_Entrprise_Postal_code__c")
        create_tag("crta__CT_Industry_of_activities__c")
        create_tag("crta__CT_Modalite_attribution_remun_variable__c")
        create_tag("crta__CT_Motif_CDI__c")
        create_tag("crta__CT_Motif_recours_CDD__c")
        create_tag("crta__CT_Nom_Responsable_hierarchique__c")
        create_tag("crta__CT_Nom_demandeur_DAR__c")
        create_tag("crta__CT_Number_of_opened_positions__c")
        create_tag("crta__CT_Organization_name__c")
        create_tag("crta__CT_Prevu_au_budget__c")
        create_tag("crta__CT_Publiee_Site_web_externe__c")
        create_tag("crta__CT_Salary__c")
        create_tag("crta__CT_Scope__c")
        create_tag("crta__CT_date_validation_DAR__c")
        create_tag("crta__Categorie__c")
        create_tag("crta__Contrainte__c")
        create_tag("crta__Contrat__c")
        create_tag("crta__Date_limite_de_reponse__c")
        create_tag("crta__Date_max_du_positionnement__c")
        create_tag("crta__Diffusion_de_l_offre__c")
        create_tag("crta__Duree_ouverture_mois__c")
        create_tag("crta__Email_du_recruteur__c")
        create_tag("crta__Etat__c")
        create_tag("crta__Filiere__c")
        create_tag("crta__Origine_de_l_offre__c")
        create_tag("crta__Status__c")
        create_tag("crta__Temps_de_travail_en_pourcentage__c")
        create_tag("crtarecr__Archived__c")
        create_tag("crtarecr__BU1__c")
        create_tag("crtarecr__BU2__c")
        create_tag("crtarecr__Contract_type__c")
        create_tag("crtarecr__Contractual_status__c")
        create_tag("crtarecr__Employment_start_date__c")
        create_tag("crtarecr__End_date_of_Intranet_Site_publication__c")
        create_tag("crtarecr__End_date_of_Jobboard_publication__c")
        create_tag("crtarecr__End_date_of_Website_publication__c")
        create_tag("crtarecr__Envisaged_annual_gross_remuneration__c")
        create_tag("crtarecr__HR_Function__c")
        create_tag("crtarecr__Max_salary__c")
        create_tag("crtarecr__Max_term_months__c")
        create_tag("crtarecr__Min_salary__c")
        create_tag("crtarecr__Min_term_months__c")
        create_tag("crtarecr__Period__c")
        create_tag("crtarecr__Published_on_Jobboards__c")
        create_tag("crtarecr__Published_on_Website__c")
        create_tag("crtarecr__Recruiter_Firstname__c")
        create_tag("crtarecr__Recruiter_Lastname__c")
        create_tag("crtarecr__Recruitment_Request_Date__c")
        create_tag("crtarecr__Recruitment_request_status__c")
        create_tag("crtarecr__Reference__c")
        create_tag("crtarecr__Required_Experience_Level__c")
        create_tag("crtarecr__Required_diploma_level__c")
        create_tag("crtarecr__Start_date_of_Intranet_Site_publication__c")
        create_tag("crtarecr__Start_date_of_Jobboard_publication__c")
        create_tag("crtarecr__Start_date_of_Website_publication__c")
        create_tag("crtarecr__Type_of_employment__c")
        create_tag("crtarecr__Weekly_Working_Time_in_hours__c")
        create_tag("Position__c")
        create_tag("M_tier__c")
        create_tag("Famille_de_m_tier__c")
        create_tag("Sourcing_Auto__c")
        create_tag("Mots_Clefs__c")
        create_tag("Disponibilit_imm_diate__c")
        create_tag("Date_de_d_but__c")
        create_tag("Date_de_fin__c")
        create_tag("Fourchette_de_salaire__c")
        create_tag("Secteur_d_activite__c")
        create_tag("Domaine_d_activite__c")
        create_tag("Niveau_d_exp_rience_attendu__c")
        create_tag("Emploi_Type__c")
        create_tag("Pays__c")
        create_tag("Mobilit_R_gion__c")
        create_tag("Mobilit_Pays__c")
        create_tag("Entit_juridique__c")
        create_tag("Site_de_diffusion_de_l_offre__c")
        create_tag("Compte_de_rattachement__c")

        # metadatas
        job["metadatas"] = []

        def create_metadata(field_name: str):
            """
            Create metadata in job if `field_name` is not `None`

            Args:
                field_name (str): Field name in Crosstalent. For example : `crta__Fiche_de_poste__c`
            """
            metadata_name = "crosstalent_{}".format(field_name)
            metadata_value = data.get(field_name)
            if metadata_value is not None:
                metadata = dict(name=metadata_name, value=metadata_value)
                job["metadatas"].append(metadata)

        ## Add metadata
        create_metadata("crta__CT_Code__c")
        create_metadata("crta__CT_Designation__c")
        create_metadata("crta__CT_Support__c")
        create_metadata("crta__CT_Work_hours__c")
        create_metadata("crta__Direction__c")
        create_metadata("crta__Entite__c")
        create_metadata("crta__Entity__c")
        create_metadata("crta__Fiche_de_poste__c")
        create_metadata("crta__Location__c")
        create_metadata("crta__Nb_positionnements__c")
        create_metadata("crta__Rattachement_hierarchique__c")
        create_metadata("crta__Recruteur__c")
        create_metadata("crta__Texte_statut__c")
        create_metadata("crta__Valeur_statut__c")
        create_metadata("crtarecr__Approver_1__c")
        create_metadata("crtarecr__Approver_2__c")
        create_metadata("crtarecr__Job_based_in__c")
        create_metadata("crtarecr__Link_of_the_form_on_job_offer__c")
        create_metadata("crtarecr__Nb_of_applications_still_to_be_processed__c")
        create_metadata("Besoin_client__c")

        return job


class PushProfileAction(PushProfileBaseAction):
    auth: OAuth2PasswordCredentialsBody
    subdomain: str = Field(
        ...,
        description="Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.salesforce.com/ABC`",
    )

    def format(self, data):
        firstname = data["info"].get("first_name")
        lastname = data["info"].get("last_name")
        email = data["info"].get("email")
        key = data["key"]

        # Case if lastname is not defined after CV parsing
        # This field is required for Crosstalent
        if lastname is None or lastname == "":
            data["info"]["last_name"] = "N/A"

        # Case if email is not defined after CV parsing
        # This field is required for Crosstalent
        if email is None or email == "":
            if firstname is not None and lastname is not None:
                email = f"{firstname}.{lastname}@vulcain.com"
            else:
                email = f"{key}@vulcain.com"
            data["info"]["email"] = email

        return data

    def push(self, data):
        profile = next(data)

        # Prepare request
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "POST"
        push_profile_request.url = f"https://{self.subdomain}.salesforce.com/services/apexrest/crta/HrFlowCreateProfile"
        push_profile_request.auth = self.auth
        push_profile_request.json = profile
        prepared_request = push_profile_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            raise RuntimeError(
                f"Push profile to Crosstalent failed : `{response.content}`"
            )
