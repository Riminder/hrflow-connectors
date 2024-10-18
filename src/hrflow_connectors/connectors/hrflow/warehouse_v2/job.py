import html
import re
import typing as t
from logging import LoggerAdapter

from hrflow import Hrflow
from pydantic import Field

from hrflow_connectors.connectors.hrflow.schemas import HrFlowJob
from hrflow_connectors.core.warehouse_v2 import (
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseWriteAction,
)

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
    def __init__(self, *args, client_response: t.Dict):
        self.client_response = client_response


class AuthParameters(ParametersModel):
    api_secret: str = Field(
        ...,
        description="API Key used to access HrFlow.ai API",
        repr=False,
        field_type=FieldType.Auth,
    )
    api_user: str = Field(
        ...,
        description="User email used to access HrFlow.ai API",
        field_type=FieldType.Auth,
    )


class CreateJobParameters(ParametersModel):
    board_key: str = Field(
        ..., description="HrFlow.ai board key", field_type=FieldType.QueryParam
    )
    enrich_with_parsing: bool = Field(
        False,
        description="When enabled jobs are enriched with HrFlow.ai parsing",
        field_type=FieldType.Other,
    )


class UpdateJobParameters(ParametersModel):
    board_key: str = Field(
        ..., description="HrFlow.ai board key", field_type=FieldType.QueryParam
    )


class ArchiveJobParameters(ParametersModel):
    board_key: str = Field(
        ..., description="HrFlow.ai board key", field_type=FieldType.QueryParam
    )


def remove_html_tags(text: str) -> str:
    return re.sub("<[^<]+?>", "", text)


def enrich_job_with_parsing(hrflow_client: Hrflow, job: t.Dict) -> None:
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
    for field in [
        "skills",
        "languages",
        "tasks",
        "certifications",
        "courses",
        "interests",
    ]:
        if job.get(field) is None:
            job[field] = []

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
    action_parameters: CreateJobParameters,
    jobs: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_jobs = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    for job in jobs:
        if action_parameters.enrich_with_parsing:
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
            board_key=action_parameters.board_key, job_json=job
        )
        if response["code"] >= 400:
            adapter.error(
                "Failed to index job with reference={} board_key={} response={}".format(
                    job['reference'],action_parameters.board_key, response
                )
            )
            failed_jobs.append(job)
    return failed_jobs


def update(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: UpdateJobParameters,
    jobs: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_jobs = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    for job in jobs:
        job_reference = job["reference"]
        job_key = job["key"]
        if job_reference is None and job_key is None:
            adapter.error(
                "can't update job without reference or key"
            )
            failed_jobs.append(job)

        response = hrflow_client.job.storing.edit(
            board_key=action_parameters.board_key,job_json=job
        )
        if response["code"] >= 400:
            adapter.error(
                "Failed to update job with reference={} board_key={} response={}".format(
                   job["reference"], action_parameters.board_key, response
                )
            )
            failed_jobs.append(job)
        
    return failed_jobs


def archive(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: UpdateJobParameters,
    jobs: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_jobs = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    for job in jobs:
        job_reference = job["reference"]

        if not job_reference:
            adapter.error("can't archive job without reference")
            failed_jobs.append(job)
            continue
        response = hrflow_client.job.storing.archive(board_key=action_parameters.board_key,reference=job_reference)
        if response["code"] >= 400:
            adapter.error(
                "Failed to archive job with reference={} board_key={} response={}".format(
                    job_reference, action_parameters.board_key, response
                )
            )
            failed_jobs.append(job)
        
    return failed_jobs


HrFlowJobWarehouse = Warehouse(
    name="HrFlow.ai Jobs",
    data_schema=HrFlowJob,
    data_type=DataType.job,
    create=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=CreateJobParameters,
        function=create,
    ),
    update=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=UpdateJobParameters,
        function=update,
    ),
    archive=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=ArchiveJobParameters,
        function=archive,
    ),
)
