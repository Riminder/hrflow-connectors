import typing as t

from hrflow_connectors.connectors.crosstalent.warehouse import CrosstalentJobWarehouse
from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.core.connector import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)

# region format_job


def get_job_location(crosstalent_location: t.Dict) -> t.Dict:
    lat = crosstalent_location.get("crta__Location__Latitude__s")
    lat = float(lat) if lat is not None else lat

    lng = crosstalent_location.get("crta__Location__Longitude__s")
    lng = float(lng) if lng is not None else lng

    concatenate = []
    for field in ["Lieu__c", "crta__CT_Country__c", "Region__c", "crta__CT_City__c"]:
        if crosstalent_location.get(field):
            concatenate.append(crosstalent_location.get(field))

    postcode = crosstalent_location.get("crta__CT_Postal_code__c")
    if postcode is None:
        postcode = crosstalent_location.get("crta__Postal_Code__c")

    if postcode != None:
        concatenate.append(postcode)

    return dict(lat=lat, lng=lng, text=" ".join(concatenate))


def get_sections(crosstalent_job: t.Dict) -> t.List[t.Dict]:
    sections = []

    section = crosstalent_job.get("crta__CT_Description__c")
    if section is not None:
        sections.append(
            dict(
                name="crosstalent-sections-crta__CT_Description__c",
                title="Description",
                description="Descriptif du poste",
            )
        )

    section = crosstalent_job.get("Profil_recherche__c")
    if section is not None:
        sections.append(
            dict(
                name="crosstalent-sections-Profil_recherche__c",
                title="Profil recherché",
                description="Profile recherché",
            )
        )

    return sections


def get_tags(crosstalent_job: t.Dict) -> t.List[t.Dict]:
    job = crosstalent_job

    t = lambda name, value: dict(name=name, value=value)
    return [
        t("crosstalent_Disponible_sous__c", job.get("Disponible_sous__c")),
        t(
            "crosstalent_crta__CT_Designation__c",
            job.get("crosstalent_crta__CT_Designation__c"),
        ),
        t(
            "crosstalent_crtarecr__Start_date_of_Website_publication__c",
            job.get("crtarecr__Start_date_of_Website_publication__c"),
        ),
        t(
            "crosstalent_Site_de_diffusion_de_l_offre__c",
            job.get("Site_de_diffusion_de_l_offre__c"),
        ),
        t("crosstalent_M_tier__c", job.get("M_tier__c")),
        t(
            "crosstalent_Niveau_d_exp_rience_attendu__c",
            job.get("Niveau_d_exp_rience_attendu__c"),
        ),
        t(
            "crosstalent_Sous_Secteur_d_activite__c",
            job.get("Sous_Secteur_d_activite__c"),
        ),
        t("crosstalent_compensation-currency", job.get("currency")),
        t("crosstalent_Langue_de_diffusion__c", job.get("Langue_de_diffusion__c")),
        t(
            "crosstalent_Numero_d_offre_automatique__c",
            job.get("Numero_d_offre_automatique__c"),
        ),
        t(
            "crosstalent_crtarecr__Published_on_Website__c",
            job.get("crtarecr__Published_on_Website__c"),
        ),
        t("crosstalent_Sourcing_Auto__c", job.get("Sourcing_Auto__c")),
        t("crosstalent_Mots_Clefs__c", job.get("Mots_Clefs__c")),
        t(
            "crosstalent_Disponibilit_imm_diate__c",
            job.get("Disponibilit_imm_diate__c"),
        ),
        t("crosstalent_Date_de_d_but__c", job.get("Date_de_d_but__c")),
        t("crosstalent_Date_de_fin__c", job.get("Date_de_fin__c")),
        t("crosstalent_Mobilit_R_gion__c", job.get("Mobilit_R_gion__c")),
    ]


def get_languages(crosstalent_job: t.Dict) -> t.List[t.Dict]:
    languages = []

    language_name = crosstalent_job.get("crtarecr__Language_1__c")
    language_level = crosstalent_job.get("crtarecr__Language_1__c")
    if language_name is not None:
        language = dict(name=language_name, value=language_level)
        languages.append(language)

    language_name = crosstalent_job.get("crtarecr__Language_2__c")
    language_level = crosstalent_job.get("crtarecr__Language_2__c")
    if language_name is not None:
        language = dict(name=language_name, value=language_level)
        languages.append(language)

    language_name = crosstalent_job.get("crtarecr__Language_3__c")
    language_level = crosstalent_job.get("crtarecr__Language_3__c")
    if language_name is not None:
        language = dict(name=language_name, value=language_level)
        languages.append(language)

    return languages


def get_metadatas(crosstalent_job: t.Dict) -> t.List[t.Dict]:
    metadatas = []

    metadata_value = crosstalent_job.get("Site_Corporate_Introduction__c")
    metadata = dict(name="Site_Corporate_Introduction__c", value=metadata_value)
    metadatas.append(metadata)

    metadata_value = crosstalent_job.get("Site_Corporate_Conclusion__c")
    metadata = dict(name="Site_Corporate_Conclusion__c", value=metadata_value)
    metadatas.append(metadata)

    return metadatas


def format_job(crosstalent_job: t.Dict) -> t.Dict:
    job = dict(
        name=crosstalent_job.get("Name", "Undefined"),
        reference=crosstalent_job.get("Id"),
        created_at=crosstalent_job.get("CreatedDate"),
        updated_at=crosstalent_job.get("LastModifiedDate"),
        location=get_job_location(crosstalent_job),
        url=crosstalent_job.get("Lien_du_formulaire_sur_offre_du_site__c"),
        summary=None,
        sections=get_sections(crosstalent_job),
        tags=get_tags(crosstalent_job),
        languages=get_languages(crosstalent_job),
        metadatas=get_metadatas(crosstalent_job),
    )
    return job


# endregion


DESCRIPTION = "METTRE UNE DESCRIPTION."  # TODO: Mettre une description.

Crosstalent = Connector(
    name="Crosstalent",
    description=DESCRIPTION,
    url="https://www.crosstalent.eu/fr/",
    actions=[
        ConnectorAction(
            name="pull_jobs",
            type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the ***Crosstalent*** API and send them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_default_format(
                "PullJobsActionParameters", format=format_job
            ),
            origin=CrosstalentJobWarehouse,
            target=HrFlowJobWarehouse,
        )
    ],
)
