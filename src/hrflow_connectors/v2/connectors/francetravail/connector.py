import typing as t

from hrflow_connectors.v2.connectors.francetravail.warehouse import (
    FranceTravailWarehouse,
)
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_job_location(pole_emploi_location: t.Optional[t.Dict]) -> t.Dict:
    if pole_emploi_location is None:
        return dict(lat=None, lng=None, text="")

    return dict(
        lat=pole_emploi_location.get("latitude"),
        lng=pole_emploi_location.get("longitude"),
        text=" ".join(
            [
                value
                for field in ["libelle", "codePostal"]
                if (value := pole_emploi_location.get(field)) is not None
            ]
        ),
    )


def get_sections(job: t.Dict) -> t.List[t.Dict]:
    sections = []
    if "entreprise" in job and "description" in job["entreprise"]:
        sections.append(
            dict(
                name="company_description",
                title="Company Description",
                description=job["entreprise"]["description"],
            )
        )
    return sections


def get_requirements(pole_emploi_formations: t.List[t.Dict]) -> t.Optional[str]:
    requirements = "\n".join(
        [
            f"{formation['niveauLibelle']} en {formation['domaineLibelle']}"
            for formation in pole_emploi_formations
            if formation.get("niveauLibelle") and formation.get("domaineLibelle")
        ]
    )
    return requirements if requirements else None


def get_tags(job: t.Dict) -> t.List[t.Dict]:
    contact = job.get("contact", {})
    salary = job.get("salaire", {})
    tags = []

    # Helper function to create tag dictionaries
    create_tag = lambda name, value: dict(name=name, value=value)

    tags = [
        create_tag("rome_code", job.get("romeCode")),
        create_tag("rome_label", job.get("romeLibelle")),
        create_tag("contract_nature", job.get("natureContrat")),
        create_tag("contract_type", job.get("typeContratLibelle")),
        create_tag("accessible_to_disabled", job.get("accessibleTH")),
        create_tag("is_apprenticeship", job.get("alternance")),
        create_tag("experience_required", job.get("experienceExige") == "E"),
        create_tag("experience_description", job.get("experienceLibelle")),
        create_tag("salary_description", salary.get("libelle")),
        create_tag("working_hours_description", job.get("dureeTravailLibelle")),
        create_tag("working_hours_type", job.get("dureeTravailLibelleConverti")),
        create_tag("number_of_positions", job.get("nombrePostes")),
        create_tag("qualification_label", job.get("qualificationLibelle")),
        create_tag("sector_of_activity", job.get("secteurActiviteLibelle")),
        create_tag("company_name", job.get("entreprise", {}).get("nom")),
        create_tag("company_description", job.get("entreprise", {}).get("description")),
        create_tag("company_website", job.get("entreprise", {}).get("url")),
        create_tag("recruiter_name", contact.get("nom")),
        create_tag("recruiter_email", contact.get("courriel")),
        create_tag("recruiter_phone", contact.get("telephone")),
        create_tag("recruiter_website", contact.get("urlRecruteur")),
        create_tag("application_url", job.get("urlPostulation")),
    ]
    return tags


def format_job(
    pole_emploi_job: t.Dict,
) -> t.Dict:
    job = dict(
        name=pole_emploi_job.get("intitule"),
        reference=pole_emploi_job.get("id"),
        created_at=pole_emploi_job.get("dateCreation"),
        updated_at=pole_emploi_job.get("dateActualisation"),
        location=get_job_location(pole_emploi_job.get("lieuTravail")),
        url=pole_emploi_job.get("origineOffre", {}).get("urlOrigine"),
        summary=pole_emploi_job.get("description"),
        requirements=get_requirements(pole_emploi_job.get("formations", [])),
        skills=[
            dict(name=skill["libelle"], value=None, type="hard")
            for skill in pole_emploi_job.get("competences", [])
        ],
        languages=[
            dict(name=language["libelle"], value=None)
            for language in pole_emploi_job.get("langues", [])
        ],
        sections=get_sections(pole_emploi_job),
        tags=get_tags(pole_emploi_job),
    )
    return job


DESCRIPTION = (
    "France Travail, formerly Pôle emploi until 31 December 2023, is a public"
    " administrative body responsible for employment in France. It was created on 19"
    " December 2008 from the merger of the Agence nationale pour l'emploi and the"
    " Association pour l'emploi dans l'industrie et le commerce."
)


def format_archive(
    pole_emploi_job: t.Dict,
) -> t.Dict:
    return dict(reference=pole_emploi_job.get("id"))


FranceTravail = Connector(
    name="France Travail (ex: Pole Emploi)",
    type=ConnectorType.JobBoard,
    subtype="francetravail",
    description=DESCRIPTION,
    url="https://www.francetravail.fr/",
    warehouse=FranceTravailWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.archive, Entity.job, Direction.inbound, format=format_archive),
    ),
)
