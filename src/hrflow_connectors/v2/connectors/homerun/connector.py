import typing as t

from hrflow_connectors.v2.connectors.homerun.warehouse import HomerunWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def format_job(homerun_job: t.Dict) -> t.Dict:
    hrflow_job = dict(
        reference=homerun_job["id"],
        title=homerun_job["title"],
        url=homerun_job["job_url"],
        summary=homerun_job["description"],
        sections=[
            {
                "name": "description",
                "title": "Description",
                "content": homerun_job["description"],
            },
            {
                "name": "page_content",
                "title": "Page Content",
                "content": homerun_job["page_content"],
            },
        ],
        location=dict(
            text=homerun_job["location"]["address"],
            fields=dict(
                country=homerun_job["location"]["country"],
                text=homerun_job["location"]["address"],
                postcode=homerun_job["location"]["postal_code"],
                city=homerun_job["location"]["city"],
                state=homerun_job["location"]["region"],
            ),
        ),
        created_at=homerun_job["created_at"],
        tags=[
            dict(name="department", value=homerun_job["department"]["name"]),
            dict(name="is_remote", value=homerun_job["is_remote"]),
            dict(
                name="total_candidate_count", value=homerun_job["total_candidate_count"]
            ),
            dict(name="status", value=homerun_job["status"]),
        ],
    )

    return hrflow_job


def format_job_for_archive(homerun_job: t.Dict) -> t.Dict:
    return dict(reference=homerun_job["id"])


DESCRIPTION = (
    "Homerun is a simple and lightweight Applicant Tracking System (ATS) for"
    " collaboration in your recruitment tasks."
)

Homerun = Connector(
    name="Homerun",
    type=ConnectorType.ATS,
    subtype="homerun",
    url="https://www.homerun.co/",
    description=DESCRIPTION,
    warehouse=HomerunWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.archive, Entity.job, Direction.inbound, format=format_job_for_archive
        ),
    ),
)
