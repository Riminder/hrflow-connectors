import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.connectors.poleemploi.warehouse import PoleEmploiJobWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


def get_job_location(pole_emploi_location: t.Union[t.Dict, None]) -> t.Dict:
    if pole_emploi_location is None:
        return dict(lat=None, lng=None, text="")

    lat = pole_emploi_location.get("latitude")
    lat = float(lat) if lat is not None else lat

    lng = pole_emploi_location.get("longitude")
    lng = float(lng) if lng is not None else lng

    return dict(
        lat=lat,
        lng=lng,
        text=" ".join(
            [
                pole_emploi_location.get(field)
                for field in ["libelle", "codePostal"]
                if pole_emploi_location.get(field)
            ]
        ),
    )


def get_sections(pole_emploi_job: t.Dict) -> t.List[t.Dict]:
    sections = []
    if (
        "entreprise" in pole_emploi_job
        and "description" in pole_emploi_job["entreprise"]
    ):
        sections.append(
            dict(
                name="pole_emploi_company_description",
                title="Company Description",
                description=pole_emploi_job["entreprise"]["description"],
            )
        )
    return sections


def get_tags(pole_emploi_job: t.Dict) -> t.List[t.Dict]:
    job = pole_emploi_job
    contact = job.get("contact", {})
    salary = job.get("salaire", {})
    tags = []

    t = lambda name, value: dict(name=name, value=value)
    tags = [
        t("pole_emploi_romeCode", job.get("romeCode")),
        t("pole_emploi_romeLibelle", job.get("romeLibelle")),
        t("pole_emploi_appellationLibelle", job.get("appellationLibelle")),
        t("pole_emploi_contractNature", job.get("natureContrat")),
        t("pole_emploi_contractType", job.get("typeContratLibelle")),
        t("pole_emploi_experience", job.get("experienceLibelle")),
        t("pole_emploi_salary", salary.get("libelle")),
        t("pole_emploi_working_hours", job.get("dureeTravailLibelle")),
        t("pole_emploi_qualification", job.get("qualificationLibelle")),
        t("pole_emploi_secteurActivite", job.get("secteurActiviteLibelle")),
        t("pole_emploi_contact-name", contact.get("nom")),
        t("pole_emploi_contact-email", contact.get("courriel")),
        t("pole_emploi_contact-phone", contact.get("telephone")),
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
        url=None,
        summary=pole_emploi_job.get("description"),
        sections=get_sections(pole_emploi_job),
        tags=get_tags(pole_emploi_job),
    )
    return job


DESCRIPTION = (
    "Thanks to the Job Offers API, you can access all the job offers available on the"
    " Pôle emploi website at any time and in real time."
)

PoleEmploi = Connector(
    name="PoleEmploi",
    type=ConnectorType.JobBoard,
    description=DESCRIPTION,
    url="https://www.pole-emploi.fr/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs via the ***Offres d'emploi v2*** API from the Pôle"
                " emploi website based on selection criteria set in the and send them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=PoleEmploiJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
