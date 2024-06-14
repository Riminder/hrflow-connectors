import typing as t
from logging import LoggerAdapter

from pydantic import Field

from hrflow_connectors.connectors.admen.schemas import AdmenJob, AdmenProfile
from hrflow_connectors.connectors.admen.utils.mysql_utils import (
    connect_to_database,
    insert_object,
    query_database,
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


class ReadJobsParameters(ParametersModel):
    # Database connection details
    db_host: str = Field(
        ...,
        description="The hostname of the database server",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_port: int = Field(
        ...,
        description="The port of the database server",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_name: str = Field(
        ...,
        description="The name of the database",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_user: str = Field(
        ...,
        description="The username to connect to the database",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_password: str = Field(
        ...,
        description="The password to connect to the database",
        repr=False,
        field_type=FieldType.Auth,
    )
    limit: int = Field(
        100,
        description="The number of jobs to retrieve",
        field_type=FieldType.QueryParam,
    )
    offset: int = Field(
        0,
        description="The offset to start retrieving jobs",
        field_type=FieldType.QueryParam,
    )


class ReadProfilesParameters(ParametersModel):
    # Database connection details
    db_host: str = Field(
        ...,
        description="The hostname of the database server",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_port: int = Field(
        ...,
        description="The port of the database server",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_name: str = Field(
        ...,
        description="The name of the database",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_user: str = Field(
        ...,
        description="The username to connect to the database",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_password: str = Field(
        ...,
        description="The password to connect to the database",
        repr=False,
        field_type=FieldType.Auth,
    )
    limit: int = Field(
        100,
        description="The number of profiles to retrieve",
        field_type=FieldType.QueryParam,
    )
    offset: int = Field(
        0,
        description="The offset to start retrieving profiles",
        field_type=FieldType.QueryParam,
    )


class WriteProfilesParameters(ParametersModel):
    # Database connection details
    db_host: str = Field(
        ...,
        description="The hostname of the database server",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_port: int = Field(
        ...,
        description="The port of the database server",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_name: str = Field(
        ...,
        description="The name of the database",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_user: str = Field(
        ...,
        description="The username to connect to the database",
        repr=False,
        field_type=FieldType.Auth,
    )
    db_password: str = Field(
        ...,
        description="The password to connect to the database",
        repr=False,
        field_type=FieldType.Auth,
    )


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    # Connect to the database
    connection = connect_to_database(
        parameters.db_host,
        parameters.db_port,
        parameters.db_user,
        parameters.db_password,
        parameters.db_name,
    )
    if connection:
        # Table name
        jobs_table = "TANNONCES"
        # Query the database
        query = (
            f"SELECT * FROM {jobs_table} LIMIT {parameters.limit} OFFSET"
            f" {parameters.offset}"
        )
        jobs = query_database(connection, query)
        connection.close()
        return jobs


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    # Connect to the database
    connection = connect_to_database(
        parameters.db_host,
        parameters.db_port,
        parameters.db_user,
        parameters.db_password,
        parameters.db_name,
    )
    if connection:
        # Table names
        profiles_table = "PERSONNES"
        experiences_table = "EXPERIENCES_PROFESSIONNELLES"
        # Query the database
        query = (
            f"SELECT * FROM {profiles_table} LIMIT {parameters.limit} OFFSET"
            f" {parameters.offset}"
        )
        profiles = query_database(connection, query)
        for profile in profiles:
            ID_PERSONNE = profile["ID_PERSONNE"]
            # Query the experience Table
            experiences_query = (
                f"SELECT * FROM {experiences_table} WHERE ID_PERSONNE = {ID_PERSONNE}"
            )
            experiences = query_database(connection, experiences_query)
            profile["experiences"] = experiences
        connection.close()
        return profiles


def write_profiles(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    # Connect to the database
    connection = connect_to_database(
        parameters.db_host,
        parameters.db_port,
        parameters.db_user,
        parameters.db_password,
        parameters.db_name,
    )
    if connection:
        # Table name
        profiles_table = "PERSONNES"
        # Insert profiles into the database
        for profile in profiles:
            insert_object(connection, profiles_table, profile)
        connection.close()
        return profiles


AdmenJobWarehouse = Warehouse(
    name="AD-MEN Jobs",
    data_schema=AdmenJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
    ),
)

AdmenProfileWarehouse = Warehouse(
    name="AD-MEN Profiles",
    data_schema=AdmenProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read_profiles,
    ),
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write_profiles,
    ),
)
