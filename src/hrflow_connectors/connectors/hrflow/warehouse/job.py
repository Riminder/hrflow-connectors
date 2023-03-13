import html
import re
import typing as t
from logging import LoggerAdapter

from hrflow import Hrflow
from pydantic import Field

from hrflow_connectors.connectors.hrflow.schemas import HrFlowJob
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseWriteAction,
)

LIST_JOBS_LIMIT = 30

LABEL_TO_JOB_FIELD = dict(
    Course="courses",
    Task="tasks",
    Certification="certifications",
    Language="languages",
)
SKILL_LABEL_TO_TYPE = dict(Skill=None, HardSkill="hard", SoftSkill="soft")


class JobParsingException(Exception):
    def __init__(self, *args, client_response: t.Dict):
        self.client_response = client_response


class WriteJobParameters(ParametersModel):
    api_secret: str = Field(
        ...,
        description="X-API-KEY used to access HrFlow.ai API",
        repr=False,
        field_type=FieldType.Auth,
    )
    api_user: str = Field(
        ...,
        description="X-USER-EMAIL used to access HrFlow.ai API",
        field_type=FieldType.Auth,
    )
    board_key: str = Field(
        ..., description="HrFlow.ai board key", field_type=FieldType.QueryParam
    )
    sync: bool = Field(
        True,
        description="When enabled only pushed jobs will remain in the board",
        field_type=FieldType.Other,
    )
    update_content: bool = Field(
        False,
        description="When enabled jobs already present in the board are updated",
        field_type=FieldType.Other,
    )
    enrich_with_parsing: bool = Field(
        False,
        description="When enabled jobs are enriched with HrFlow.ai parsing",
        field_type=FieldType.Other,
    )


def remove_html_tags(text: str) -> str:
    return re.sub("<[^<]+?>", "", text)


def enrich_job_with_parsing(hrflow_client: Hrflow, job: t.Dict) -> None:
    concatenate = []
    summary = job.get("summary")
    if summary:
        concatenate.append(summary)

    for section in job.get("sections") or []:
        description = section.get("description")
        if description:
            concatenate.append(description)

    concatenated = "\n".join(concatenate)
    cleaned = html.unescape(remove_html_tags(concatenated)).strip()
    if cleaned == "":
        return

    response = hrflow_client.document.parsing.post(text=cleaned)
    if response["code"] >= 400:
        raise JobParsingException("Failed to parse job", client_response=response)

    entities, parsed_text = response["data"]["ents"], response["data"]["text"]
    for field in ["skills", "languages", "certifications", "courses", "tasks"]:
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


def write(
    adapter: LoggerAdapter, parameters: WriteJobParameters, jobs: t.Iterable[t.Dict]
) -> t.List[t.Dict]:
    failed_jobs = []
    hrflow_client = Hrflow(
        api_secret=parameters.api_secret, api_user=parameters.api_user
    )
    if parameters.sync is True:
        jobs = list(jobs)
        references_to_push = set(
            [job.get("reference") for job in jobs if job.get("reference")]
        )

        references_in_board = set()
        page = 1
        while True:
            response = hrflow_client.job.storing.list(
                board_keys=[parameters.board_key], limit=LIST_JOBS_LIMIT, page=page
            )
            if response["code"] >= 400:
                adapter.error(
                    "Failed to list jobs in board board_key={} "
                    "limit={} page={} response={}".format(
                        parameters.board_key, LIST_JOBS_LIMIT, page, response
                    )
                )
                raise Exception("Failed to list jobs in board")
            references_in_board.update(
                [
                    job.get("reference")
                    for job in response["data"]
                    if job.get("reference")
                ]
            )
            # FIXME why + 1 ? max_page + 1 in get_all_job_pages_from_board
            if page == response["meta"]["maxPage"] + 1:
                break
            page += 1

        references_to_archive = references_in_board - references_to_push
        adapter.info(
            "Sync mode enabled. Archiving {} items from board".format(
                len(references_to_archive)
            )
        )
        for reference in references_to_archive:
            response = hrflow_client.job.indexing.archive(
                board_key=parameters.board_key, reference=reference, is_archive=1
            )
            if response["code"] >= 400:
                adapter.error(
                    "Failed to archive job in board board_key={} "
                    "reference={} response={}".format(
                        parameters.board_key, reference, response
                    )
                )
                raise Exception("Failed to archive job")
        adapter.info("Archiving finished")

    jobs_to_write = []
    if parameters.enrich_with_parsing:
        adapter.info("Enrich with parsing enabled. Starting parsing")
        for job in jobs:
            try:
                enrich_job_with_parsing(hrflow_client, job)
                jobs_to_write.append(job)
            except JobParsingException as e:
                adapter.error(
                    "Failed to parse job response={}".format(e.client_response)
                )
                failed_jobs.append(job)
        adapter.info("Parsing finished")
    else:
        jobs_to_write = jobs

    for job in jobs_to_write:
        reference = job.get("reference")
        if reference is None:
            response = hrflow_client.job.indexing.add_json(
                board_key=parameters.board_key, job_json=job
            )
            if response["code"] >= 400:
                adapter.error(
                    "Failed to index job with no reference "
                    "board_key={} response={}".format(parameters.board_key, response)
                )
                failed_jobs.append(job)
            continue

        response = hrflow_client.job.indexing.get(
            board_key=parameters.board_key, reference=reference
        )
        if "Unable to find object: job" in response["message"]:
            response = hrflow_client.job.indexing.add_json(
                board_key=parameters.board_key, job_json=job
            )
            if response["code"] >= 400:
                adapter.error(
                    "Failed to index job board_key={} reference={} response={}".format(
                        parameters.board_key, reference, response
                    )
                )
                failed_jobs.append(job)
                continue
        elif response["code"] == 200:
            archived_at = response["data"].get("archived_at")
            job_key = response["data"]["key"]
            if archived_at is None:
                if parameters.update_content:
                    response = hrflow_client.job.indexing.edit(
                        board_key=parameters.board_key, key=job_key, job_json=job
                    )
                    if response["code"] >= 400:
                        adapter.error(
                            "Failed to edit job board_key={} "
                            "reference={} response={}".format(
                                parameters.board_key, reference, response
                            )
                        )
                        failed_jobs.append(job)
                        continue
            else:
                response = hrflow_client.job.indexing.archive(
                    board_key=parameters.board_key, reference=reference, is_archive=0
                )
                if response["code"] >= 400:
                    adapter.error(
                        "Failed to unarchive job board_key={} "
                        "reference={} response={}".format(
                            parameters.board_key, reference, response
                        )
                    )
                    failed_jobs.append(job)
                    continue
                response = hrflow_client.job.indexing.edit(
                    board_key=parameters.board_key, key=job_key, job_json=job
                )
                if response["code"] >= 400:
                    adapter.error(
                        "Failed to edit job board_key={} "
                        "reference={} response={}".format(
                            parameters.board_key, reference, response
                        )
                    )
                    failed_jobs.append(job)
                    continue
        else:
            adapter.error(
                "Failed to get job from board board_key={} "
                "reference={} response={}".format(
                    parameters.board_key, reference, response
                )
            )
            failed_jobs.append(job)
            continue

    return failed_jobs


HrFlowJobWarehouse = Warehouse(
    name="HrFlow.ai Jobs",
    data_schema=HrFlowJob,
    data_type=DataType.job,
    write=WarehouseWriteAction(parameters=WriteJobParameters, function=write),
)
