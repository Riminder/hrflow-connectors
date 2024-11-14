import html
import re
import typing as t
from logging import LoggerAdapter

from hrflow import Hrflow
from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.hrflow.aisles.common import AuthParameters
from hrflow_connectors.v2.core.hrflow.schemas import HrFlowJob
from hrflow_connectors.v2.core.warehouse import Aisle, Criterias, WriteOperation, merge

LIST_JOBS_LIMIT = 30

LABEL_TO_JOB_FIELD = dict(
    language="languages",
    task="tasks",
    certification="certifications",
    course="courses",
    interest="interests",
)
SKILL_LABEL_TO_TYPE = dict(Skill=None, skill_hard="hard", skill_soft="soft")


class JobParsingException(Exception):
    def __init__(self, *args, client_response: dict):
        self.client_response = client_response


class CreateCriterias(Struct):
    board_key: Annotated[
        str,
        Meta(
            description="HrFlow.ai board key",
        ),
    ]
    enrich_with_parsing: Annotated[
        bool,
        Meta(
            description="When enabled jobs are enriched with HrFlow.ai parsing",
        ),
    ] = False


class UpdateCriterias(Struct):
    board_key: Annotated[str, Meta(description="HrFlow.ai board key")]


class ArchiveCriterias(Struct):
    board_key: Annotated[str, Meta(description="HrFlow.ai board key")]


def remove_html_tags(text: str) -> str:
    return re.sub("<[^<]+?>", "", text)


def enrich_job_with_parsing(hrflow_client: Hrflow, job: dict) -> None:
    concatenate = []
    summary = job.get("summary")
    if summary:
        concatenate.append(summary)

    for section in job.get("sections") or []:
        content = []
        title = section.get("title")
        if title:
            content.append(title)
        description = section.get("description")
        if description:
            content.append(description)
        if content:
            concatenate.append("\n".join(content))

    concatenated = "\n\n".join(concatenate)
    cleaned = html.unescape(remove_html_tags(concatenated)).strip()
    if cleaned == "":
        return

    response = hrflow_client.text.parsing.post(texts=[cleaned])
    if response["code"] >= 400:
        raise JobParsingException("Failed to parse job", client_response=response)

    entities, parsed_text = response["data"][0]["entities"], response["data"][0]["text"]
    for attribute in [
        "skills",
        "languages",
        "tasks",
        "certifications",
        "courses",
        "interests",
    ]:
        if job.get(attribute) is None:
            job[attribute] = []

    for entitiy in entities:
        label = entitiy["label"]
        entity_text = parsed_text[entitiy["start"] : entitiy["end"]]

        if label in LABEL_TO_JOB_FIELD:
            job_field = LABEL_TO_JOB_FIELD[label]
            if next(
                (
                    element
                    for element in job[job_field]
                    if element["name"] == entity_text
                ),
                False,
            ):
                continue
            job[job_field].append(dict(name=entity_text, value=None))
        elif label in SKILL_LABEL_TO_TYPE:
            skill_type = SKILL_LABEL_TO_TYPE[label]
            if next(
                (
                    skill
                    for skill in job["skills"]
                    if skill["name"] == entity_text and skill["type"] == skill_type
                ),
                False,
            ):
                continue
            job["skills"].append(dict(name=entity_text, type=skill_type, value=None))

    return


def create(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: CreateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    failed_jobs = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    for job in items:
        if parameters.enrich_with_parsing:
            adapter.info("Starting parsing for job without reference")
            try:
                enrich_job_with_parsing(hrflow_client, job)
                adapter.info("Parsing finished")
            except JobParsingException as e:
                adapter.error(
                    "Failed to parse job response={}".format(e.client_response)
                )
                failed_jobs.append(job)
                continue
        response = hrflow_client.job.storing.add_json(
            board_key=parameters.board_key, job_json=job
        )
        if response["code"] >= 400:
            adapter.error(
                "Failed to index job with reference={} board_key={} response={}".format(
                    job["reference"], parameters.board_key, response
                )
            )
            failed_jobs.append(job)
    return failed_jobs


def update(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    failed_jobs = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    for job in items:
        job_reference = job.get("reference")
        job_key = job.get("key")
        if job_reference is None and job_key is None:
            adapter.error("can't update job without reference or key")
            failed_jobs.append(job)

        response = hrflow_client.job.storing.edit(
            board_key=parameters.board_key, job_json=job
        )
        if response["code"] >= 400:
            if "Unable to find object: job" in response["message"]:
                adapter.error(
                    "Failed to update job with reference={} board_key={} response={}"
                    .format(job["reference"], parameters.board_key, response)
                )
                continue
            adapter.error(
                "Failed to update job with reference={} board_key={} response={}"
                .format(job["reference"], parameters.board_key, response)
            )
            failed_jobs.append(job)

    return failed_jobs


def archive(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    failed_jobs = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    for job in items:
        job_reference = job.get("reference")

        if not job_reference:
            adapter.error("can't archive job without reference")
            failed_jobs.append(job)
            continue
        response = hrflow_client.job.storing.archive(
            board_key=parameters.board_key, reference=job_reference
        )
        if response["code"] >= 400:
            if "Unable to find object: job" in response["message"]:
                adapter.error(
                    "Failed to archive job with reference={} board_key={} response={}"
                    .format(job["reference"], parameters.board_key, response)
                )
                continue
            adapter.error(
                "Failed to archive job with reference={} board_key={} response={}"
                .format(job_reference, parameters.board_key, response)
            )
            failed_jobs.append(job)

    return failed_jobs


JobsAisle = Aisle(
    name=Entity.job,
    write=WriteOperation(
        function=merge(create=create, update=update, archive=archive),
        criterias=Criterias(
            create=CreateCriterias, update=UpdateCriterias, archive=ArchiveCriterias
        ),
    ),
    schema=HrFlowJob,
)
