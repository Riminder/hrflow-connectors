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
    response = hrflow_client.profile.indexing.get(
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
            current_profile = hrflow_client.profile.indexing.get(
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

            response = hrflow_client.profile.indexing.edit(
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
            response = hrflow_client.profile.indexing.add_json(
                source_key=parameters.source_key, profile_json=profile
            )
            if response["code"] // 100 != 2:
                adapter.error("Failed to add profile with response={}".format(response))
                failed.append(profile)
    return failed


def write_parsing(
    adapter: LoggerAdapter,
    parameters: WriteProfileParsingParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=parameters.api_secret, api_user=parameters.api_user
    )
    for profile in profiles:
        if parameters.only_insert and hrflow_client.profile.indexing.get(
            source_key=parameters.source_key, reference=profile["reference"]
        ).get("data"):
            adapter.info(
                "Mode only_insert is activated. Profile with reference={} already"
                " in source. Skipping...".format(profile["reference"])
            )
            continue

        response = hrflow_client.profile.parsing.add_file(
            source_key=parameters.source_key,
            profile_file=profile["resume"]["raw"],
            profile_content_type=profile["resume"]["content_type"],
            reference=profile["reference"],
            tags=profile["tags"],
            metadatas=profile["metadatas"],
            created_at=profile["created_at"],
        )
        if response["code"] != 202:
            adapter.error(
                "Failed to parse profile with reference={} response={}".format(
                    profile["reference"], response
                )
            )
            failed.append(profile)
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
