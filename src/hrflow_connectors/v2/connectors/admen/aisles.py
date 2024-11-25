import typing as t
from logging import LoggerAdapter

from msgspec import Meta, Struct
from smbprotocol.open import (
    CreateDisposition,
    CreateOptions,
    FilePipePrinterAccessMask,
    ImpersonationLevel,
    Open,
    ShareAccess,
)
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.admen.schemas import AdmenMission, AdmenProfile
from hrflow_connectors.v2.connectors.admen.utils.mysql_utils import (
    connect_to_database,
    insert_object,
    query_database,
    update_object,
)
from hrflow_connectors.v2.connectors.admen.utils.smb_utils import (
    establish_smb_connection,
)
from hrflow_connectors.v2.core.common import Entity, Mode
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
)


class AuthParameters(Struct):
    db_host: Annotated[
        str,
        Meta(
            description="The hostname of the database server",
        ),
    ]
    db_port: Annotated[
        int,
        Meta(
            description="The port of the database server",
        ),
    ]
    db_name: Annotated[
        str,
        Meta(
            description="The name of the database",
        ),
    ]
    db_user: Annotated[
        str,
        Meta(
            description="The username to connect to the database",
        ),
    ]
    db_password: Annotated[
        str,
        Meta(
            description="The password to connect to the database",
        ),
    ]


class ReadJobsParameters(Struct):
    limit: Annotated[
        t.Optional[int],
        Meta(
            description="The maximum number of items to fetch",
        ),
    ] = None


class ReadProfilesParameters(Struct):
    share_server: Annotated[
        t.Optional[str],
        Meta(
            description="The hostname of the network share server",
        ),
    ] = None
    share_name: Annotated[
        t.Optional[str],
        Meta(
            description="The name of the network share",
        ),
    ] = None
    share_username: Annotated[
        t.Optional[str],
        Meta(
            description="The username to connect to the network share",
        ),
    ] = None
    share_password: Annotated[
        t.Optional[str],
        Meta(
            description="The password to connect to the network share",
        ),
    ] = None
    limit: Annotated[
        t.Optional[int],
        Meta(
            description="The maximum number of items to fetch",
        ),
    ] = None


class WriteProfilesParameters(Struct):
    pass


def generic_job_read(
    mode: Mode,
):
    def read_jobs(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadJobsParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[t.Dict]:
        jobs_table = "MISSIONS"
        query_conditions = {
            Mode.create: "WHERE DATE_MAJ = DATE_SAISIE",
            Mode.update: "WHERE DATE_MAJ > DATE_SAISIE",
            Mode.archive: "WHERE SOFT_DELETED = 1",
        }
        query = f"SELECT * FROM {jobs_table} {query_conditions[mode]}"
        if parameters.limit:
            query += f" LIMIT {parameters.limit}"

        adapter.info(f"Executing query: {query}")

        # Connect to the database
        connection = connect_to_database(
            auth_parameters.db_host,
            auth_parameters.db_port,
            auth_parameters.db_user,
            auth_parameters.db_password,
            auth_parameters.db_name,
        )
        if connection:
            jobs = query_database(connection, query)
            job_count = len(jobs) if jobs else 0
            adapter.info(f"Found {job_count} jobs in the database")
            if jobs:
                yield from jobs
            connection.close()

    return read_jobs


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


def generic_profile_read(
    mode: Mode,
):
    """Factory function to read profiles based on the specified mode."""

    def read_profiles(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadProfilesParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
        max_retries: int = 5,
        retry_delay: int = 5,
    ) -> t.Iterable[t.Dict]:
        profiles_table = "PERSONNES"
        experiences_table = "EXPERIENCES_PROFESSIONNELLES"
        documents_table = "DOCUMENTS"

        query_conditions = {
            Mode.create: "WHERE DATE_MAJ = DATE_CREATION",
            Mode.update: "WHERE DATE_MAJ > DATE_CREATION",
            Mode.archive: "WHERE SOFT_DELETED = 1",
        }
        query = f"SELECT * FROM {profiles_table} {query_conditions[mode]}"
        if parameters.limit:
            query += f" LIMIT {parameters.limit}"

        adapter.info(f"Executing query: {query}")

        connection = connect_to_database(
            auth_parameters.db_host,
            auth_parameters.db_port,
            auth_parameters.db_user,
            auth_parameters.db_password,
            auth_parameters.db_name,
        )
        if connection:
            profiles = query_database(connection, query)

            # Try to establish an SMB connection
            smb_connected, smb_tree, smb_session, smb_connection = (
                False,
                None,
                None,
                None,
            )
            if (
                parameters.share_server
                and parameters.share_name
                and parameters.share_username
                and parameters.share_password
            ):
                # Establish SMB connection
                smb_connected, smb_tree, smb_session, smb_connection = (
                    establish_smb_connection(
                        adapter,
                        parameters.share_server,
                        parameters.share_username,
                        parameters.share_password,
                        parameters.share_name,
                        max_retries,
                        retry_delay,
                    )
                )

            if profiles:
                for profile in profiles:
                    ID_PERSONNE = profile["ID_PERSONNE"]

                    # Fetch experiences
                    experiences_query = (
                        f"SELECT * FROM {experiences_table} WHERE ID_PERSONNE ="
                        f" {ID_PERSONNE}"
                    )
                    profile["experiences"] = query_database(
                        connection, experiences_query
                    )

                    # Fetch resume
                    documents_query = (
                        f"SELECT * FROM {documents_table} WHERE ID_PERSONNE ="
                        f" '{ID_PERSONNE}' LIMIT 1"
                    )
                    document = query_database(connection, documents_query)
                    if document and smb_connected:
                        profile["resume"] = fetch_and_save_document(
                            adapter,
                            smb_tree,
                            document[0]["PATHNAME"].replace("/", "\\"),
                        )
                    else:
                        profile["resume"] = None

                    yield profile

            # Ensure all connections are closed
            if connection:
                connection.close()

            if smb_connected:
                if smb_tree:
                    smb_tree.disconnect()
                if smb_session:
                    smb_session.disconnect()
                if smb_connection:
                    smb_connection.disconnect()

    return read_profiles


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    # Connect to the database
    connection = connect_to_database(
        auth_parameters.db_host,
        auth_parameters.db_port,
        auth_parameters.db_user,
        auth_parameters.db_password,
        auth_parameters.db_name,
    )
    if connection:
        # Table name
        profiles_table = "PERSONNES"
        experiences_table = "EXPERIENCES_PROFESSIONNELLES"
        # Insert profiles into the database
        for profile in items:
            profile_experiences = profile.pop("experiences", [])
            if not profile.get("scsync_modified"):
                profile["scsync_modified"] = 0
            try:
                inserted_id = insert_object(
                    adapter, connection, profiles_table, profile
                )
                if inserted_id:
                    for experience in profile_experiences:
                        experience["ID_PERSONNE"] = inserted_id
                        insert_object(
                            adapter, connection, experiences_table, experience
                        )
            except Exception as e:
                adapter.error(f"Failed to insert profile into the database: {e}")
                failed_profiles.append(profile)
        connection.close()
    return failed_profiles


def generic_update():
    def update(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: WriteProfilesParameters,
        items: t.Iterable[t.Dict],
    ) -> t.List[t.Dict]:
        failed_items = []
        profiles_table = "PERSONNES"
        experiences_table = "EXPERIENCES_PROFESSIONNELLES"

        connection = connect_to_database(
            auth_parameters.db_host,
            auth_parameters.db_port,
            auth_parameters.db_user,
            auth_parameters.db_password,
            auth_parameters.db_name,
        )
        if connection:
            for item in items:
                ID_PERSONNE = item.pop("ID_PERSONNE")
                experiences = item.pop("experiences", [])
                where_clause = {"ID_PERSONNE": ID_PERSONNE}
                update_result = update_object(
                    adapter, connection, profiles_table, item, where_clause
                )
                if not update_result:
                    failed_items.append(item)
                for experience in experiences:
                    query = f"""
                    SELECT * FROM {experiences_table}
                    WHERE INTITULE_POSTE = %s AND ID_PERSONNE = %s
                    """
                    params = (experience["INTITULE_POSTE"], ID_PERSONNE)
                    existing_experience = query_database(connection, query, params)
                    if existing_experience:
                        where_clause = {
                            "ID_PERSONNE": ID_PERSONNE,
                            "INTITULE_POSTE": experience["INTITULE_POSTE"],
                        }
                        update_result = update_object(
                            adapter,
                            connection,
                            experiences_table,
                            experience,
                            where_clause,
                        )
                        if not update_result:
                            adapter.error(f"Failed to update experience: {experience}")
                    else:
                        experience["ID_PERSONNE"] = ID_PERSONNE
                        insert_object(
                            adapter, connection, experiences_table, experience
                        )

            connection.close()
        return failed_items

    return update


JobsAisle = Aisle(
    name=Entity.job,
    schema=AdmenMission,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=generic_job_read(Mode.create),
            update=generic_job_read(Mode.update),
            archive=generic_job_read(Mode.archive),
        ),
    ),
)

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=AdmenProfile,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=generic_profile_read(Mode.create),
            update=generic_profile_read(Mode.update),
            archive=generic_profile_read(Mode.archive),
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
            update=WriteProfilesParameters,
            archive=WriteProfilesParameters,
        ),
        function=merge(
            create=write_profiles,
            update=generic_update(),
            archive=generic_update(),
        ),
    ),
)
