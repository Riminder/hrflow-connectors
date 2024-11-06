import os
import time
import typing as t
from logging import LoggerAdapter

from pydantic import Field
from smbprotocol.connection import Connection
from smbprotocol.open import (
    CreateDisposition,
    CreateOptions,
    FilePipePrinterAccessMask,
    ImpersonationLevel,
    Open,
    ShareAccess,
)
from smbprotocol.session import Session
from smbprotocol.tree import TreeConnect

from hrflow_connectors.connectors.admen.schemas import AdmenMission, AdmenProfile
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


class DatabaseConnectionParameters(ParametersModel):
    db_host: str = Field(
        ...,
        description="The hostname of the database server",
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
        field_type=FieldType.Auth,
    )
    db_user: str = Field(
        ...,
        description="The username to connect to the database",
        field_type=FieldType.Auth,
    )
    db_password: str = Field(
        ...,
        description="The password to connect to the database",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadJobsParameters(DatabaseConnectionParameters):
    pass


class ReadProfilesParameters(DatabaseConnectionParameters):
    share_server: str = Field(
        ...,
        description="The hostname of the network share server",
        field_type=FieldType.Auth,
    )
    share_name: str = Field(
        ...,
        description="The name of the network share",
        field_type=FieldType.Auth,
    )
    share_username: str = Field(
        ...,
        description="The username to connect to the network share",
        field_type=FieldType.Auth,
    )
    share_password: str = Field(
        ...,
        description="The password to connect to the network share",
        repr=False,
        field_type=FieldType.Auth,
    )

    share_domain: str = Field(
        ...,
        description="The domain to connect to the network share",
        field_type=FieldType.Auth,
    )


class WriteProfilesParameters(DatabaseConnectionParameters):
    pass


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
        jobs_table = "MISSIONS"
        # Query the database
        query = f"SELECT * FROM {jobs_table}"
        jobs = query_database(connection, query)
        for job in jobs:
            yield job
        connection.close()


def fetch_and_save_document(adapter, tree, file_path):
    try:
        # Open the document and read its content
        document = Open(tree, file_path)
        document.create(
            impersonation_level=ImpersonationLevel.Impersonation,
            desired_access=FilePipePrinterAccessMask.FILE_READ_DATA,
            file_attributes=0,
            share_access=ShareAccess.FILE_SHARE_READ,
            create_disposition=CreateDisposition.FILE_OPEN,
            create_options=CreateOptions.FILE_NON_DIRECTORY_FILE,
        )
        file_size = document.end_of_file
        read_offset = 0
        read_length = file_size
        file_data = document.read(read_offset, read_length)
        document.close()

        # Return the raw content and content type
        return {"raw": file_data, "content_type": "pdf"}

    except Exception as e:
        adapter.error(f"An error occurred while fetching the document: {e}")
        return None


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
    max_retries: int = 5,
    retry_delay: int = 5,
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
        documents_table = "DOCUMENTS"

        # Query the database
        query = f"SELECT * FROM {profiles_table}"
        profiles = query_database(connection, query)

        # Try to establish an SMB connection
        smb_connected = False
        retries = 0
        smb_connection = smb_session = smb_tree = None
        while retries < max_retries and not smb_connected:
            try:
                # Establish a connection to the SMB server
                smb_connection = Connection(
                    guid=os.urandom(16), server_name=parameters.share_server, port=445
                )
                smb_connection.connect()

                # Create and authenticate a session
                smb_session = Session(
                    smb_connection,
                    parameters.share_username,
                    parameters.share_password,
                    parameters.share_domain,
                )
                smb_session.connect()

                # Connect to the specified share
                smb_tree = TreeConnect(
                    smb_session,
                    f"\\\\{parameters.share_server}\\{parameters.share_name}",
                )
                smb_tree.connect()

                smb_connected = True
            except Exception as e:
                adapter.error(f"An error occurred while connecting to SMB: {e}")
                retries += 1
                if retries < max_retries:
                    adapter.error(
                        f"Retrying SMB connection in {retry_delay} seconds..."
                        f" ({retries}/{max_retries})"
                    )
                    time.sleep(retry_delay)
                else:
                    adapter.error(
                        "Max retries for SMB connection reached. Will not fetch"
                        " documents."
                    )

        for profile in profiles:
            ID_PERSONNE = profile["ID_PERSONNE"]

            # Query the experience Table
            experiences_query = (
                f"SELECT * FROM {experiences_table} WHERE ID_PERSONNE = {ID_PERSONNE}"
            )
            experiences = query_database(connection, experiences_query)
            profile["experiences"] = experiences

            # Query the documents Table
            documents_query = (
                f"SELECT * FROM {documents_table} WHERE ID_PERSONNE = '{ID_PERSONNE}'"
                " LIMIT 1;"
            )
            document = query_database(connection, documents_query)
            if document and smb_connected:
                resume = fetch_and_save_document(
                    adapter,
                    smb_tree,
                    document[0]["PATHNAME"].replace("/", "\\"),
                )
                profile["resume"] = resume
            else:
                profile["resume"] = None

            yield profile

        # Close SMB connections if they were established
        if smb_connected:
            smb_tree.disconnect()
            smb_session.disconnect()
            smb_connection.disconnect()

        connection.close()


def write_profiles(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
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
            if not profile.get("scsync_modified"):
                profile["scsync_modified"] = 0
            try:
                insert_object(connection, profiles_table, profile)
            except Exception as e:
                adapter.error(f"Failed to insert profile into the database: {e}")
                failed_profiles.append(profile)
        connection.close()
    return failed_profiles


AdmenJobWarehouse = Warehouse(
    name="AD-MEN Jobs",
    data_schema=AdmenMission,
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
