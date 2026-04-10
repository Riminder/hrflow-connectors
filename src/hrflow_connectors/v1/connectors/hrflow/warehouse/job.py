import html
import re
import typing as t
from logging import LoggerAdapter

import requests
from hrflow import Hrflow
from pydantic import Field

from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseWriteAction,
)
from hrflow_connectors.v1.connectors.hrflow.schemas import HrFlowJob

LIST_JOBS_LIMIT = 30

LABEL_TO_JOB_FIELD = dict(
    course="courses",
    task="tasks",
    certification="certifications",
    language="languages",
)
SKILL_LABEL_TO_TYPE = dict(Skill=None, skill_hard="hard", skill_soft="soft")


def smart_update(base: t.Dict, updates: t.Dict) -> t.Dict:
    """Merge parsed results into a job dict using append-if-empty-per-item
    strategy.

    For list fields (skills, languages, ...): append items from updates that
    are not already present (deduplicated by name).
    For scalar fields (summary, culture, ...): only fill if the base value is
    empty or None.
    """
    list_fields = [
        "skills",
        "languages",
        "certifications",
        "courses",
        "tasks",
        "interests",
    ]
    scalar_fields = [
        "summary",
        "culture",
        "benefits",
        "responsibilities",
        "requirements",
        "interviews",
    ]

    for field in list_fields:
        parsed_items = updates.get(field) or []
        if not parsed_items:
            continue
        if base.get(field) is None:
            base[field] = []
        existing_names = {
            item.get("name", "").lower() for item in base[field] if item.get("name")
        }
        for item in parsed_items:
            if item.get("name") and item["name"].lower() not in existing_names:
                base[field].append(item)
                existing_names.add(item["name"].lower())

    for field in scalar_fields:
        parsed_value = updates.get(field)
        if parsed_value and not base.get(field):
            base[field] = parsed_value

    # Merge location if base location text is empty
    parsed_location = updates.get("location")
    if parsed_location and isinstance(parsed_location, dict):
        if base.get("location") is None:
            base["location"] = parsed_location
        elif not base["location"].get("text"):
            base["location"]["text"] = parsed_location.get("text")

    # Merge ranges (date, float) if base has none
    for range_field in ["ranges_date", "ranges_float"]:
        parsed_ranges = updates.get(range_field)
        if parsed_ranges and not base.get(range_field):
            base[range_field] = parsed_ranges

    return base


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
    enrich_with_parsing_v2: bool = Field(
        False,
        description=(
            "When enabled jobs are enriched with HrFlow.ai Atlas parsing model."
            " Uses the newer parsing API that returns a structured job object."
            " Cannot be used together with enrich_with_parsing."
        ),
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

    response = hrflow_client.text.parsing.post(texts=[cleaned])
    if response["code"] >= 400:
        raise JobParsingException("Failed to parse job", client_response=response)

    entities, parsed_text = response["data"][0]["entities"], response["data"][0]["text"]
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


def enrich_job_with_parsing_v2(api_secret: str, api_user: str, job: t.Dict) -> None:
    """Enrich a job dict using HrFlow.ai Atlas parsing model.

    Sends the concatenated job text (name + summary + sections) to the
    HrFlow.ai parsing API with output_object="job" and parsing_model="atlas".
    The API returns a fully structured job object which is then merged back
    into the original job using smart_update (append-if-empty per item).
    """
    job_text = "\n\n".join(
        "{}:\n{}".format(s.get("title", "Section"), s.get("description", ""))
        for s in job.get("sections") or [{}]
        if s.get("description")
    )
    if not job_text.strip():
        return

    hrflow_api_base_url = "https://api.hrflow.ai/v1"
    response = requests.post(
        "{}/text/parsing".format(hrflow_api_base_url),
        headers={
            "accept": "application/json",
            "X-API-KEY": api_secret,
            "X-USER-EMAIL": api_user,
        },
        json=dict(
            texts=[job_text],
            output_object="job",
            parsing_model="atlas",
        ),
        timeout=30,
    )
    if response.status_code >= 400:
        raise JobParsingException(
            "Failed to parse job with atlas model",
            client_response=dict(
                status_code=response.status_code,
                body=response.text,
            ),
        )

    data = response.json().get("data", [])
    if not data or not isinstance(data, list) or not data[0].get("job"):
        return

    parsed_job = data[0]["job"]
    parsed_job.pop("key", None)
    parsed_job.pop("board_key", None)
    parsed_job.pop("board", None)

    smart_update(job, parsed_job)


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
            response = hrflow_client.job.storing.archive(
                board_key=parameters.board_key, reference=reference
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

    if parameters.enrich_with_parsing and parameters.enrich_with_parsing_v2:
        raise ValueError(
            "enrich_with_parsing and enrich_with_parsing_v2 cannot both be enabled."
            " Please choose one enrichment method."
        )

    for job in jobs:
        reference = job.get("reference")
        if reference is None:
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
            elif parameters.enrich_with_parsing_v2:
                adapter.info("Starting atlas parsing for job without reference")
                try:
                    enrich_job_with_parsing_v2(
                        parameters.api_secret, parameters.api_user, job
                    )
                    adapter.info("Atlas parsing finished")
                except JobParsingException as e:
                    adapter.error(
                        "Failed to parse job with atlas model response={}".format(
                            e.client_response
                        )
                    )
                    failed_jobs.append(job)
                    continue
            response = hrflow_client.job.storing.add_json(
                board_key=parameters.board_key, job_json=job
            )
            if response["code"] >= 400:
                adapter.error(
                    "Failed to index job with no reference board_key={} response={}"
                    .format(parameters.board_key, response)
                )
                failed_jobs.append(job)
            continue

        response = hrflow_client.job.storing.get(
            board_key=parameters.board_key, reference=reference
        )
        if "Unable to find object: job" in response["message"]:
            if parameters.enrich_with_parsing:
                adapter.info(
                    "Starting parsing for job with reference={}".format(reference)
                )
                try:
                    enrich_job_with_parsing(hrflow_client, job)
                    adapter.info("Parsing finished")
                except JobParsingException as e:
                    adapter.error(
                        "Failed to parse job response={}".format(e.client_response)
                    )
                    failed_jobs.append(job)
                    continue
            elif parameters.enrich_with_parsing_v2:
                adapter.info(
                    "Starting atlas parsing for job with reference={}".format(reference)
                )
                try:
                    enrich_job_with_parsing_v2(
                        parameters.api_secret, parameters.api_user, job
                    )
                    adapter.info("Atlas parsing finished")
                except JobParsingException as e:
                    adapter.error(
                        "Failed to parse job with atlas model response={}".format(
                            e.client_response
                        )
                    )
                    failed_jobs.append(job)
                    continue
            response = hrflow_client.job.storing.add_json(
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
            job_key = response["data"]["key"]
            if parameters.update_content:
                response = hrflow_client.job.storing.edit(
                    board_key=parameters.board_key, key=job_key, job_json=job
                )
                if response["code"] >= 400:
                    adapter.error(
                        "Failed to edit job board_key={} reference={} response={}"
                        .format(parameters.board_key, reference, response)
                    )
                    failed_jobs.append(job)
                    continue
        else:
            adapter.error(
                "Failed to get job from board board_key={} reference={} response={}"
                .format(parameters.board_key, reference, response)
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
