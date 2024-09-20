import typing as t
from logging import LoggerAdapter

from hrflow import Hrflow
from pydantic import Field

from hrflow_connectors.connectors.hrflow.schemas import (
    HrFlowProfile,
    HrFlowProfileParsing,
)
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)


class ReadProfileParameters(ParametersModel):
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
    source_key: str = Field(
        ..., description="HrFlow.ai source key", field_type=FieldType.QueryParam
    )
    profile_key: str = Field(
        ..., description="HrFlow.ai profile key", field_type=FieldType.QueryParam
    )


class WriteProfileParameters(ParametersModel):
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
    source_key: str = Field(
        ..., description="HrFlow.ai source key", field_type=FieldType.QueryParam
    )
    edit: bool = Field(
        False,
        description="When enabled the profile must exist in the source",
        field_type=FieldType.Other,
    )
    only_edit_fields: t.Optional[t.List[str]] = Field(
        ...,
        description=(
            "List of attributes to use for the edit operation e.g. ['tags',"
            " 'metadatas']"
        ),
        field_type=FieldType.Other,
    )


class WriteProfileParsingParameters(ParametersModel):
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
    source_key: str = Field(
        ..., description="HrFlow.ai source key", field_type=FieldType.Other
    )
    only_insert: bool = Field(
        False,
        description=(
            "When enabled the profile is written only if it doesn't exist in the source"
        ),
        field_type=FieldType.Other,
    )


def read(
    adapter: LoggerAdapter,
    parameters: ReadProfileParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.List[t.Dict]:
    hrflow_client = Hrflow(
        api_secret=parameters.api_secret, api_user=parameters.api_user
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


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfileParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=parameters.api_secret, api_user=parameters.api_user
    )
    for profile in profiles:
        if parameters.edit:
            current_profile = hrflow_client.profile.storing.get(
                source_key=parameters.source_key, reference=profile["reference"]
            ).get("data")
            if not current_profile:
                adapter.warning(
                    "Mode edit is activated and profile with reference={} not found"
                    " in source. Failing for profile...".format(profile["reference"])
                )
                failed.append(profile)
                continue
            if parameters.only_edit_fields:
                edit = {
                    field: profile.get(field) for field in parameters.only_edit_fields
                }
            else:
                edit = profile
            profile_to_index = {**current_profile, **edit}

            response = hrflow_client.profile.storing.edit(
                source_key=parameters.source_key,
                key=current_profile["key"],
                profile_json=profile_to_index,
            )
            if response["code"] != 200:
                adapter.error(
                    "Failed to edit profile with reference={} key={} response={}"
                    .format(
                        profile_to_index["key"], profile_to_index["reference"], response
                    )
                )
                failed.append(profile)
        else:
            response = hrflow_client.profile.storing.add_json(
                source_key=parameters.source_key, profile_json=profile
            )
            if response["code"] // 100 != 2:
                adapter.error("Failed to add profile with response={}".format(response))
                failed.append(profile)
    return failed


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


def write_parsing(
    adapter: LoggerAdapter,
    parameters: WriteProfileParsingParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=parameters.api_secret, api_user=parameters.api_user
    )

    source_response = hrflow_client.source.get(key=parameters.source_key)

    for profile in profiles:
        if parameters.only_insert and hrflow_client.profile.storing.get(
            source_key=parameters.source_key, reference=profile["reference"]
        ).get("data"):
            adapter.info(
                "Mode only_insert is activated. Profile with reference={} already"
                " in source. Skipping...".format(profile["reference"])
            )
            continue

        if profile.get("resume") is None:
            indexing_response = hrflow_client.profile.storing.add_json(
                source_key=parameters.source_key, profile_json=profile
            )
            if indexing_response["code"] != 201:
                adapter.error(
                    "Failed to index profile with reference={} response={}".format(
                        profile["reference"], indexing_response
                    )
                )
                failed.append(profile)
            continue

        parsing_response = hrflow_client.profile.parsing.add_file(
            source_key=parameters.source_key,
            profile_file=profile["resume"]["raw"],
            profile_content_type=profile["resume"]["content_type"],
            reference=profile["reference"],
            tags=profile["tags"],
            metadatas=profile["metadatas"],
            created_at=profile["created_at"],
        )
        if parsing_response["code"] not in [202, 201]:  # 202: Accepted, 201: Created
            adapter.error(
                "Failed to parse profile with reference={} response={}".format(
                    profile["reference"], parsing_response
                )
            )
            failed.append(profile)
            continue

        # check if sync_parsing is enabled
        if source_response["code"] != 200:
            adapter.warning(
                "Failed to get source with key={} response={}, won't be able to update"
                " profile parsed with profile json".format(
                    parameters.source_key, source_response
                )
            )
        elif source_response["data"]["sync_parsing"] is True:
            current_profile = parsing_response["data"]["profile"]
            profile_result = hydrate_profile(current_profile, profile)

            edit_response = hrflow_client.profile.storing.edit(
                source_key=parameters.source_key,
                key=profile_result["key"],
                profile_json=profile_result,
            )
            if edit_response["code"] != 200:
                adapter.warning(
                    "Failed to update profile after parsing, reference={}"
                    " response={}".format(profile["reference"], edit_response)
                )
                failed.append(profile)
                continue

    return failed


HrFlowProfileWarehouse = Warehouse(
    name="HrFlow.ai Profiles",
    data_schema=HrFlowProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(parameters=ReadProfileParameters, function=read),
    write=WarehouseReadAction(parameters=WriteProfileParameters, function=write),
)


HrFlowProfileParsingWarehouse = Warehouse(
    name="HrFlow.ai Profile Parsing",
    data_schema=HrFlowProfileParsing,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfileParsingParameters, function=write_parsing
    ),
)
