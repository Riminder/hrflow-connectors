import typing as t

from hrflow_connectors.v2.connectors.adzuna.warehouse import AdzunaWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_tags(adzuna_job: t.Dict) -> t.List[t.Dict]:
    t = lambda name, value: dict(name=name, value=value)
    tags = [
        t("contract_type", adzuna_job.get("contract_type")),
        t("contract_time", adzuna_job.get("contract_time")),
        t("salary_min", adzuna_job.get("salary_min")),
        t("salary_max", adzuna_job.get("salary_max")),
        t(
            "salary_is_predicted",
            adzuna_job.get("salary_is_predicted", 0) == 1,
        ),
        t("category", adzuna_job.get("category", {}).get("label")),
        t("company", adzuna_job.get("company", {}).get("display_name")),
    ]
    return tags


def format_job(
    adzuna_job: t.Dict,
) -> t.Dict:
    job = dict(
        name=adzuna_job.get("title"),
        reference=str(adzuna_job.get("id")),
        created_at=adzuna_job.get("created"),
        location=dict(
            lat=adzuna_job.get("latitude"),
            lng=adzuna_job.get("longitude"),
            text=adzuna_job.get("location", {}).get("display_name", ""),
        ),
        url=adzuna_job.get("redirect_url"),
        summary=adzuna_job.get("description"),
        sections=(
            [
                dict(
                    name="full_description",
                    title="Full Description",
                    description=adzuna_job.get("full_description"),
                )
            ]
            if adzuna_job.get("full_description")
            else []
        ),
        tags=get_tags(adzuna_job),
    )
    return job


def format_archive(adzuna_job: t.Dict) -> t.Dict:
    return dict(reference=str(adzuna_job.get("id")))


DESCRIPTION = (
    "Find Every Job, Everywhere with Adzuna\n\nAdzuna is a smarter, more transparent"
    " job search engine that helps you dodge the thousands of irrelevant jobs so you"
    " can zero in on the right role faster."
)
Adzuna = Connector(
    name="Adzuna",
    type=ConnectorType.JobBoard,
    subtype="adzuna",
    description=DESCRIPTION,
    url="https://www.adzuna.fr/",
    warehouse=AdzunaWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.archive, Entity.job, Direction.inbound, format=format_archive),
    ),
)
