import typing as t
from logging import LoggerAdapter

from hrflow import Hrflow
from pydantic import Field

from hrflow_connectors.connectors.hrflow.schemas import (
    HrFlowProfile,
    HrFlowProfileParsing,
)
from hrflow_connectors.core.warehouse_v2 import (
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)


class AuthParameters(ParametersModel):
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


class ReadProfileParameters(ParametersModel):
    source_key: str = Field(
        ..., description="HrFlow.ai source key", field_type=FieldType.QueryParam
    )
    profile_key: str = Field(
        ..., description="HrFlow.ai profile key", field_type=FieldType.QueryParam
    )


class CreateProfileParameters(ParametersModel):
    source_key: str = Field(
        ..., description="HrFlow.ai source key", field_type=FieldType.QueryParam
    )


class UpdateProfileParameters(ParametersModel):
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


class ArchiveProfileParameters(ParametersModel):
    source_key: str = Field(
        ..., description="HrFlow.ai source key", field_type=FieldType.QueryParam
    )


class CreateProfileParsingParameters(ParametersModel):
    source_key: str = Field(
        ..., description="HrFlow.ai source key", field_type=FieldType.Other
    )


def read(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: ReadProfileParameters,
) -> t.List[t.Dict]:
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    response = hrflow_client.profile.storing.get(
        source_key=action_parameters.source_key, key=action_parameters.profile_key
    )
    if "Unable to find object" in response["message"]:
        adapter.info(
            "No profile found for source_key={} profile_key={} response={}".format(
                action_parameters.source_key, action_parameters.profile_key, response
            )
        )
        return []
    elif response["code"] >= 400:
        adapter.error(
            "Failed to get profile source_key={} profile_key={} response={}".format(
                action_parameters.source_key, action_parameters.profile_key, response
            )
        )
        raise Exception("Failed to get profile")
    return [response["data"]]


def create(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: CreateProfileParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )
    for profile in profiles:
        response = hrflow_client.profile.storing.add_json(
            source_key=action_parameters.source_key, profile_json=profile
        )
        if response["code"] // 100 != 2:
            adapter.error("Failed to add profile with response={}".format(response))
            failed.append(profile)
    return failed


# TODO: implement update
def update(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: UpdateProfileParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    return []


# TODO: implement archive
def archive(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: ArchiveProfileParameters,
) -> t.List[t.Dict]:
    return []


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


def create_parsing(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    action_parameters: CreateProfileParsingParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=auth_parameters.api_secret, api_user=auth_parameters.api_user
    )

    source_response = hrflow_client.source.get(key=action_parameters.source_key)

    for profile in profiles:
        if profile.get("resume") is None:
            indexing_response = hrflow_client.profile.storing.add_json(
                source_key=action_parameters.source_key, profile_json=profile
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
            source_key=action_parameters.source_key,
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
                    action_parameters.source_key, source_response
                )
            )
        elif source_response["data"]["sync_parsing"] is True:
            current_profile = parsing_response["data"]["profile"]
            profile_result = hydrate_profile(current_profile, profile)

            edit_response = hrflow_client.profile.storing.edit(
                source_key=action_parameters.source_key,
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
    read=WarehouseReadAction(
        parameters=ReadProfileParameters,
        action_parameters=ReadProfileParameters,
        function=read,
    ),
    create=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=CreateProfileParameters,
        function=create,
    ),
    update=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=UpdateProfileParameters,
        function=update,
    ),
    archive=WarehouseReadAction(
        auth_parameters=AuthParameters,
        action_parameters=ArchiveProfileParameters,
        function=archive,
    ),
)

# TODO: update HrFlowProfileParsingWarehouse to new Warehouse structure
HrFlowProfileParsingWarehouse = Warehouse(
    name="HrFlow.ai Profile Parsing",
    data_schema=HrFlowProfileParsing,
    data_type=DataType.profile,
    create=WarehouseWriteAction(
        auth_parameters=AuthParameters,
        action_parameters=CreateProfileParsingParameters,
        function=create_parsing,
    ),
)
