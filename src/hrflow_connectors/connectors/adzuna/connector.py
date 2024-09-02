import typing as t

from hrflow_connectors.connectors.adzuna.warehouse import AdzunaJobWarehouse
from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


def get_job_location(adzuna_job: t.Dict) -> t.Dict:
    lat = adzuna_job.get("latitude")
    lat = float(lat) if lat is not None else lat

    lng = adzuna_job.get("longitude")
    lng = float(lng) if lng is not None else lng

    return dict(lat=lat, lng=lng, text=adzuna_job.get("location")["display_name"])


def get_tags(adzuna_job: t.Dict) -> t.List[t.Dict]:
    job = adzuna_job
    tags = []
    salaries_are_predicted = job.get("salary_is_predicted") == "1"

    t = lambda name, value: dict(name=name, value=value)
    tags = [
        t("salary_min", job.get("salary_min")),
        t("salary_max", job.get("salary_max")),
        t("salaries_are_predicted", salaries_are_predicted),
        t("category", job.get("category", {}).get("label")),
        t("company", job.get("company", {}).get("display_name")),
    ]
    return tags


def format_job(
    adzuna_job: t.Dict,
) -> t.Dict:
    job = dict(
        name=adzuna_job.get("title"),
        reference=str(adzuna_job.get("id")),
        created_at=adzuna_job.get("created"),
        location=get_job_location(adzuna_job),
        url=adzuna_job.get("redirect_url"),
        summary=adzuna_job.get("description"),
        sections=[],
        tags=get_tags(adzuna_job),
    )
    return job


Adzuna = Connector(
    name="Adzuna",
    type=ConnectorType.JobBoard,
    description="Retrieve Adzuna's job, property and car advertisement listings.",
    url="https://www.adzuna.fr/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs via the ***Adzuna'*** API Search endpoint"
                "and send them to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=AdzunaJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
