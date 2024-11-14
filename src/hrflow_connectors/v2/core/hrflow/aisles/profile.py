import typing as t
from logging import LoggerAdapter

from hrflow import Hrflow
from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.hrflow.aisles.common import AuthParameters
from hrflow_connectors.v2.core.hrflow.schemas import HrFlowProfile
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)


class ReadAllModesCriterias(Struct):
    source_key: Annotated[str, Meta(description="HrFlow.ai source key")]
    profile_key: Annotated[
        str,
        Meta(
            description="HrFlow.ai profile key",
        ),
    ]


class CreateCriterias(Struct):
    source_key: Annotated[
        str,
        Meta(
            description="HrFlow.ai source key",
        ),
    ]


class UpdateCriterias(Struct):
    source_key: Annotated[
        str,
        Meta(
            description="HrFlow.ai source key",
        ),
    ]
    only_edit_fields: Annotated[
        t.Optional[list[str]],
        Meta(
            description=(
                "List of attributes to use for the edit operation e.g. ['tags',"
                " 'metadatas']"
            ),
        ),
    ] = None


class ArchiveCriterias(Struct):
    source_key: Annotated[
        str,
        Meta(
            description="HrFlow.ai source key",
        ),
    ]


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadAllModesCriterias,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> list[dict]:
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    response = hrflow_client.profile.storing.get(
        source_key=parameters.source_key, key=parameters.profile_key
    )
    if "Unable to find object" in response["message"]:
        adapter.info(
            "No profile found for source_key={} profile_key={} response={}".format(
                parameters.source_key, parameters.profile_key, response
            )
        )
        return []
    elif response["code"] >= 400:
        adapter.error(
            "Failed to get profile source_key={} profile_key={} response={}".format(
                parameters.source_key, parameters.profile_key, response
            )
        )
        raise Exception("Failed to get profile")
    return [response["data"]]


def merge_info(base: dict, info: dict) -> dict:
    if not info:
        return base

    info_parsed = base.get("info", {})
    existing_urls = info_parsed.get("urls", [])

    if isinstance(info.get("urls"), list):
        for new_url in info["urls"]:
            if new_url not in existing_urls:
                existing_urls.append(new_url)

    info_parsed["urls"] = existing_urls

    for key, value in info.items():
        if value and key != "location" and key != "urls":
            info_parsed[key] = value
        elif key == "location" and isinstance(value, dict) and any(value.values()):
            info_parsed[key] = value

    base["info"] = info_parsed
    return base


def merge_item(base: dict, profile: dict, item: str) -> dict:
    if not profile.get(item):
        return base

    base[item] = profile[item]
    return base


def hydrate_profile(profile_parsed: dict, profile_json: dict) -> dict:
    profile_info = profile_json.get("info", {})
    profile_enriched = merge_info(profile_parsed, profile_info)

    items_to_merge = [
        "experiences",
        "educations",
        "skills",
        "languages",
        "certifications",
        "interests",
        "tags",
        "metadatas",
    ]
    for item in items_to_merge:
        profile_enriched = merge_item(profile_enriched, profile_json, item)

    profile_enriched["text"] = profile_json.get("text") or profile_enriched.get("text")
    profile_enriched["text_language"] = profile_json.get(
        "text_language"
    ) or profile_enriched.get("text_language")
    profile_enriched["experiences_duration"] = (
        profile_json.get("experiences_duration")
        if profile_json.get("experiences_duration") is not None
        else profile_enriched.get("experiences_duration")
    )
    profile_enriched["educations_duration"] = (
        profile_json.get("educations_duration")
        if profile_json.get("educations_duration") is not None
        else profile_enriched.get("educations_duration")
    )
    return profile_enriched


def create(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: CreateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    source_response = hrflow_client.source.get(key=parameters.source_key)

    if source_response["code"] != 200:
        adapter.warning(
            "Failed to get source with"
            f" key={parameters.source_key} response={source_response}"
        )
        return failed

    for profile in items:
        if hrflow_client.profile.storing.get(
            source_key=parameters.source_key,
            reference=profile["reference"],
        ).get("data"):
            adapter.info(
                f"Can't create Profile with reference={profile['reference']} already"
                " exists"
            )
            continue

        if profile.get("resume", {}).get("raw") is None:
            adapter.info(f"Profile with reference {profile['reference']} has no resume")
            response = hrflow_client.profile.storing.add_json(
                source_key=parameters.source_key, profile_json=profile
            )
        else:
            parsing_response = hrflow_client.profile.parsing.add_file(
                source_key=parameters.source_key,
                profile_file=profile["resume"]["raw"],
                profile_content_type=profile["resume"]["content_type"],
                profile_file_name=profile["resume"].get("file_name"),
                reference=profile["reference"],
                tags=profile.get("tags", []),
                metadatas=profile.get("metadatas", {}),
                created_at=profile.get("created_at"),
            )

            if parsing_response["code"] not in [202, 201]:
                adapter.error(
                    "Failed to parse profile with"
                    f" reference={profile['reference']} response={parsing_response}"
                )
                failed.append(profile)
                continue

            if source_response["data"].get("sync_parsing"):
                current_profile = parsing_response["data"]["profile"]
                profile_result = hydrate_profile(current_profile, profile)
                response = hrflow_client.profile.storing.edit(
                    source_key=parameters.source_key,
                    key=profile_result["key"],
                    profile_json=profile_result,
                )

        if response["code"] // 100 != 2:
            adapter.error(
                "Failed to process profile with"
                f" reference={profile['reference']} response={response}"
            )
            failed.append(profile)

    return failed


def update(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    source_response = hrflow_client.source.get(key=parameters.source_key)

    for profile in items:
        current_profile = hrflow_client.profile.storing.get(
            source_key=parameters.source_key,
            reference=profile["reference"],
        ).get("data")

        if not current_profile:
            adapter.warning(
                f"Profile with reference={profile['reference']} not found in source."
                " Failing for profile..."
            )
            failed.append(profile)
            continue

        edit = (
            {field: profile.get(field) for field in parameters.only_edit_fields}
            if parameters.only_edit_fields
            else profile
        )
        profile_to_edit = {**current_profile, **edit}

        if profile.get("resume"):
            if not current_profile.get("attachments"):
                parsing_response = hrflow_client.profile.parsing.add_file(
                    source_key=parameters.source_key,
                    profile_file=profile["resume"]["raw"],
                    profile_content_type=profile["resume"]["content_type"],
                    reference=profile["reference"],
                    tags=profile["tags"],
                    metadatas=profile["metadatas"],
                    created_at=profile["created_at"],
                )

                if parsing_response["code"] not in [201, 202]:
                    adapter.error(
                        "Failed to parse profile with"
                        f" reference={profile['reference']} response={parsing_response}"
                    )
                    failed.append(profile)
                    continue

                if source_response["data"].get("sync_parsing") is True:
                    parsing_result = parsing_response["data"]["profile"]
                    profile_to_edit = hydrate_profile(parsing_result, profile_to_edit)

        response = hrflow_client.profile.storing.edit(
            source_key=parameters.source_key,
            key=current_profile["key"],
            profile_json=profile_to_edit,
        )

        if response["code"] != 200:
            adapter.error(
                "Failed to edit profile with"
                f" reference={profile_to_edit['reference']}"
                f" key={profile_to_edit['key']} response={response}"
            )
            failed.append(profile)

    return failed


def archive(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ArchiveCriterias,
    items: t.Iterable[dict],
) -> list[dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )

    for profile in items:
        profile_reference = profile.get("reference")
        if not profile_reference:
            adapter.error("can't archive profile without reference")
            failed.append(profile)
            continue
        response = hrflow_client.profile.storing.archive(
            source_key=parameters.source_key, reference=profile_reference
        )
        if response["code"] >= 400:
            if "Unable to find object: profile" in response["message"]:
                adapter.error(
                    "Failed to archive profile with reference={} source_key={}"
                    " response={}".format(
                        profile["reference"],
                        parameters.source_key,
                        response,
                    )
                )
                continue
            adapter.error(
                "Failed to archive profile with reference={} source_key={}"
                " response={}".format(
                    profile_reference, parameters.source_key, response
                )
            )
            failed.append(profile)
    return failed


ProfilesAisle = Aisle(
    name=Entity.profile,
    read=ReadOperation(
        function=merge(create=read, update=read, archive=read),
        criterias=Criterias(
            create=ReadAllModesCriterias,
            update=ReadAllModesCriterias,
            archive=ReadAllModesCriterias,
        ),
    ),
    write=WriteOperation(
        function=merge(create=create, update=update, archive=archive),
        criterias=Criterias(
            create=CreateCriterias, update=UpdateCriterias, archive=ArchiveCriterias
        ),
    ),
    schema=HrFlowProfile,
)
