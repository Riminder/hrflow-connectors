import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.connectors.poleemploi.warehouse import PoleEmploiJobWarehouse
from hrflow_connectors.core import BaseActionParameters, Connector, ConnectorAction


def get_job_location(poleemploi_location: t.Union[t.Dict, None]) -> t.Dict:
    if poleemploi_location is None:
        return dict(lat=None, lng=None, text="")

    lat = poleemploi_location.get("latitude")
    lat = float(lat) if lat is not None else lat

    lng = poleemploi_location.get("longitude")
    lng = float(lng) if lng is not None else lng

    concatenate = []
    for field in ["libelle", "codePostal"]:
        if poleemploi_location.get(field):
            concatenate.append(poleemploi_location.get(field))

    return dict(lat=lat, lng=lng, text=" ".join(concatenate))


def get_sections(poleemploi_job: t.Dict) -> t.List[t.Dict]:
    sections = []
    if (
        "entreprise" not in poleemploi_job
        or "description" not in poleemploi_job["entreprise"]
    ):
        return sections
    else:
        sections.append(
            dict(
                name="poleemploi_company_description",
                title="Company Description",
                description=poleemploi_job["entreprise"]["description"],
            )
        )
    return sections


def get_tags(poleemploi_job: t.Dict) -> t.List[t.Dict]:  # TODO: get more tags
    job = poleemploi_job
    contact = job.get("contact", {})
    salaire = job.get("salaire", {})

    t = lambda name, value: dict(name=name, value=value)
    return [
        t("poleemploi_romeCode", job.get("romeCode")),
        t("poleemploi_romeLibelle", job.get("romeLibelle")),
        t("poleemploi_appellationLibelle", job.get("appellationLibelle")),
        t("poleemploi_contratNature", job.get("natureContrat")),
        t("poleemploi_contractType", job.get("typeContratLibelle")),
        t("poleemploi_experience", job.get("experienceLibelle")),
        t("poleemploi_salary", salaire.get("libelle")),
        t("poleemploi_working_hours", job.get("dureeTravailLibelle")),
        t("poleemploi_qualification", job.get("qualificationLibelle")),
        t("poleemploi_secteurActivite", job.get("secteurActiviteLibelle")),
        t("poleemploi_contact-name", contact.get("nom")),
        t("poleemploi_contact-email", contact.get("courriel")),
        t("poleemploi_contact-phone", contact.get("telephone")),
    ]


def format_job(
    poleemploi_job: t.Dict,
) -> t.Dict:  # TODO: format job to HrFlow format before being sent to Job board.
    job = dict(
        name=poleemploi_job.get("intitule"),
        reference=poleemploi_job.get("id"),
        created_at=poleemploi_job.get("dateCreation"),
        updated_at=poleemploi_job.get("dateActualisation"),
        location=get_job_location(poleemploi_job.get("lieuTravail")),
        url=None,
        summary=poleemploi_job.get("description"),
        sections=get_sections(poleemploi_job),
        tags=get_tags(poleemploi_job),
    )
    return job


DESCRIPTION = (
    "Thanks to the Job Offers API, you can access all the job offers available on the"
    " Pôle emploi website at any time and in real time."
)

PoleEmploi = Connector(
    name="PoleEmploi",
    description=DESCRIPTION,
    url="https://www.smartrecruiters.com/",
    actions=[
        ConnectorAction(
            name="pull_jobs",
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
        ),
    ],
)
