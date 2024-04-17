import typing as t
from enum import Enum
from logging import LoggerAdapter

from hrflow import Hrflow
from pydantic import Field

from hrflow_connectors.connectors.hrflow.schemas import HrflowTracking, Role
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)


class ReadTrackingParameters(ParametersModel):
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

    role: Role = Field(
        ...,
        description="Filter by role(Role of the user rating the job)",
        field_type=FieldType.QueryParam,
    )
    actions: t.Optional[t.List[str]] = Field(
        None,
        description=(
            "Filter by actions\nThe 'action' refers to a unique identifier for a"
            " profile or job stage. This can be a specific stage ID within a CRM, ATS,"
            ' or Job site.Examples of such stages include "view," "apply," "hire," or'
            " any other stage relevant to your system."
        ),
        field_type=FieldType.QueryParam,
    )
    source_keys: t.Optional[t.List[str]] = Field(
        None,
        description=(
            "Filter by source keys\nTo filter by a specific list of sources, include"
            " the parameter source_keys and leave source_key empty,"
            " profile_key || profile_reference empty."
        ),
        field_type=FieldType.QueryParam,
    )
    source_key: t.Optional[str] = Field(
        None,
        description=(
            "To filter by a specific profile, include the parameters (source_key,"
            " profile_key || profile_reference) and leave source_keys empty."
        ),
        field_type=FieldType.QueryParam,
    )
    profile_key: t.Optional[str] = Field(
        None,
        description=(
            "Filter by profile key\nTo filter by a specific profile, include"
            "the parameters (source_key,profile_key || profile_reference) and leave"
            "source_keys empty. If you set profile_key, you must set profile_reference as empty."
        ),
        field_type=FieldType.QueryParam,
    )
    profile_reference: t.Optional[str] = Field(
        None,
        description=(
            "Filter by profile reference\nTo filter by a specific profile, include"
            " the parameters (source_key, profile_key || profile_reference) and leave"
            " source_keys empty. If you set profile_reference, you must set profile_key as empty."
        ),
        field_type=FieldType.QueryParam,
    )
    board_keys: t.Optional[t.List[str]] = Field(
        None,
        description=(
            "Filter by board keys\nTo filter ratings based on a list of jobs within a"
            " list of boards, include the parameter board_keys and leave (board_key,"
            " job_key//job_reference) empty."
        ),
        field_type=FieldType.QueryParam,
    )
    board_key: t.Optional[str] = Field(
        None,
        description=(
            "To filter by a specific job, include the parameters"
            " (board_key,job_key//job_reference) and leave board_keys empty."
        ),
        field_type=FieldType.QueryParam,
    )
    job_key: t.Optional[str] = Field(
        None,
        description=(
            "To filter by a specific job, include the parameters"
            " (board_key,job_key//job_reference) and leave board_keys empty."
        ),
        field_type=FieldType.QueryParam,
    )
    job_reference: t.Optional[str] = Field(
        None,
        description=(
            "To filter by a specific job, include the parameters"
            " (board_key,job_key//job_reference) and leave board_keys empty."
        ),
        field_type=FieldType.QueryParam,
    )
    return_profile: bool = Field(
        False,
        description="Return profile",
    )
    return_author: bool = Field(
        False,
        description="Return author",
        field_type=FieldType.QueryParam,
    )
    order_by: str = Field(
        "desc",
        description="Order by",
        field_type=FieldType.QueryParam,
    )
    sort_by: str = Field(
        "scoring",
        description="Sort by",
        field_type=FieldType.QueryParam,
    )
    created_at_min: t.Optional[str] = Field(
        None,
        description="Created at min",
        field_type=FieldType.QueryParam,
    )
    created_at_max: t.Optional[str] = Field(
        None,
        description="Created at max",
        field_type=FieldType.QueryParam,
    )
    location_lat: t.Optional[float] = Field(
        None,
        description="Location latitude",
        field_type=FieldType.QueryParam,
    )
    location_lon: t.Optional[float] = Field(
        None,
        description="Location longitude",
        field_type=FieldType.QueryParam,
    )
    location_distance: t.Optional[float] = Field(
        None,
        description="Location distance",
        field_type=FieldType.QueryParam,
    )
    use_location_address: t.Optional[bool] = Field(
        None,
        description="Filter by location address",
        field_type=FieldType.QueryParam,
    )
    use_location_experience: t.Optional[bool] = Field(
        None,
        description="Filter by location experience",
        field_type=FieldType.QueryParam,
    )
    use_location_education: t.Optional[bool] = Field(
        None,
        description="Filter by location education",
        field_type=FieldType.QueryParam,
    )
    experiences_duration_min: t.Optional[int] = Field(
        None,
        description="Filter by the minof Experiences duration",
        field_type=FieldType.QueryParam,
    )
    experiences_duration_max: t.Optional[int] = Field(
        None,
        description="Filter by the max of Experiences duration",
        field_type=FieldType.QueryParam,
    )
    educations_duration_min: t.Optional[int] = Field(
        None,
        description="Filter by the min of Educations duration",
        field_type=FieldType.QueryParam,
    )
    educations_duration_max: t.Optional[int] = Field(
        None,
        description="Filter by the max of Educations duration",
        field_type=FieldType.QueryParam,
    )


class WriteTrackingParameters(ParametersModel):
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


def read(
    adapter: LoggerAdapter,
    parameters: ReadTrackingParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.List[t.Dict]:
    hrflow_client = Hrflow(
        api_secret=parameters.api_secret, api_user=parameters.api_user
    )
    page = 1
    trackings = []
    while True:
        response = hrflow_client.tracking.get(
            role=parameters.role,
            actions=parameters.actions,
            source_keys=parameters.source_keys,
            source_key=parameters.source_key,
            profile_key=parameters.profile_key,
            profile_reference=parameters.profile_reference,
            board_keys=parameters.board_keys,
            board_key=parameters.board_key,
            job_key=parameters.job_key,
            job_reference=parameters.job_reference,
            return_profile=parameters.return_profile,
            return_author=parameters.return_author,
            page=page,
            limit=100,
            order_by=parameters.order_by,
            sort_by=parameters.sort_by,
            created_at_min=parameters.created_at_min,
            created_at_max=parameters.created_at_max,
            location_lat=parameters.location_lat,
            location_lon=parameters.location_lon,
            location_distance=parameters.location_distance,
            use_location_address=parameters.use_location_address,
            use_location_experience=parameters.use_location_experience,
            use_location_education=parameters.use_location_education,
            experiences_duration_min=parameters.experiences_duration_min,
            experiences_duration_max=parameters.experiences_duration_max,
            educations_duration_min=parameters.educations_duration_min,
            educations_duration_max=parameters.educations_duration_max,
        )
        if "Unable to find object" in response["message"]:
            adapter.info(
                "No tracking found with the given parameters: {}.\nReturning an empty"
                " list.".format(
                    {
                        key: value
                        for key, value in parameters.dict(exclude_unset=True).items()
                    }
                )
            )
            return []
        elif response["code"] >= 400:
            adapter.error(
                "Error while fetching tracking: {}.".format(response["message"])
            )
            raise Exception("Failed to get tracking")
        trackings.extend(response["data"])
        page += 1
        if page > response["meta"]["maxPage"]:
            break
    return trackings


def write(
    adapter: LoggerAdapter,
    parameters: WriteTrackingParameters,
    trackings: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    hrflow_client = Hrflow(
        api_secret=parameters.api_secret, api_user=parameters.api_user
    )
    for tracking in trackings:
        response = hrflow_client.tracking.post(**tracking)
        if response["code"] // 100 != 2:
            adapter.error("Failed to add profile with response={}".format(response))
            failed.append(tracking)
    return failed


HrFlowTrackingWarehouse = Warehouse(
    name="HrFlowTracking",
    data_schema=HrflowTracking,
    data_type=DataType.other,
    read=WarehouseReadAction(parameters=ReadTrackingParameters, function=read),
    write=WarehouseWriteAction(parameters=WriteTrackingParameters, function=write),
)
