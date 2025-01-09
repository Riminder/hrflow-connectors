import json
import typing as t
from datetime import datetime, timezone
from logging import LoggerAdapter

from msgspec import Meta, Struct
from simple_salesforce import Salesforce, SalesforceError
from typing_extensions import Annotated

from hrflow_connectors.v1.connectors.salesforce.schemas import (
    SalesforceHrFlowJob,
    SalesforceHrFlowProfile,
)
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    IncrementalTokenHandler,
    ReadOperation,
    WriteOperation,
    merge,
)

DEFAULT_LIMIT_PROFILES = 100
DEFAULT_LIMIT_JOBS = 1000
SOQL_MAX_RETURNED_ROWS = 2000

SELECT_PROFILES_SOQL = """
SELECT
    LastModifiedDate,
    Id__c,
    Dataset_Id__c,
    Date_Edition__c,
    Date_Reception__c,
    Text__c,
    Email__c,
    URLs__c,
    Name__c,
    Gender__c,
    Location_Text__c,
    Location_Lat__c,
    Location_Lng__c,
    Location_Gmaps__c,
    Phone__c,
    Timestamp__c,
    Summary__c,
    Hash_Id__c,
    Text_Language__c,
    Reference__c,
    Skills__c,
    Languages__c,
    Interests__c,
    Seniority__c,
    Location_Fields__c,
    Experiences_Duration__c,
    Educations_Duration__c,
    Archive__c,
    Picture__c,
    First_Name__c,
    Last_Name__c,
    Labels__c,
    Metadatas__c,
    Tags__c,
    Certifications__c,
    Courses__c,
    Tasks__c,
    Date_Birth__c,
    (
        SELECT
            FIELDS(CUSTOM)
        FROM HrFlow_Profile_Experiences__r
        LIMIT 200
    ),
    (
        SELECT
            FIELDS(CUSTOM)
        FROM HrFlow_Profile_Educations__r
        LIMIT 200
    ),
    (
        SELECT
            FIELDS(CUSTOM)
        FROM HrFlow_Profile_Attachments__r
        LIMIT 200
    )
FROM HrFlow_Profile__c
WHERE LastModifiedDate >= {last_modified_date}
ORDER BY LastModifiedDate, Id__c
{limit_placeholder}
"""

SELECT_JOBS_SOQL = """
SELECT
    LastModifiedDate,
    Id__c,
    Name__c,
    Slug__c,
    URL__c,
    Summary__c,
    Status__c,
    Date_Edition__c,
    Skills__c,
    Archive__c,
    Hash_Id__c,
    Location_Text__c,
    Location_Lat__c,
    Location_Lng__c,
    Tags__c,
    Board_Id__c,
    Sections__c,
    Reference__c,
    Languages__c,
    Ranges_Float__c,
    Ranges_Date__c,
    Location_Gmaps__c,
    Location_Fields__c,
    Metadatas__c,
    Certifications__c,
    Courses__c,
    Tasks__c,
    Picture__c,
    Culture__c,
    Responsibilities__c,
    Requirements__c,
    Benefits__c,
    Interviews__c
FROM HrFlow_Job__c
WHERE LastModifiedDate >= {last_modified_date}
ORDER BY LastModifiedDate, Id__c
{limit_placeholder}
"""


class AuthParameters(Struct):
    sf_username: Annotated[
        str,
        Meta(
            description="username used to access Salesforce API",
        ),
    ]
    sf_password: Annotated[
        str,
        Meta(
            description="password used to access Salesforce API",
        ),
    ]
    sf_security_token: Annotated[
        str,
        Meta(
            description=(
                "Security Token to access Salesforce API.See below for"
                " instructions: How"
                " Can I Find My Security Token and Use It in Data Loader | Salesforce"
                " Platform "
                " https://www.youtube.com/watch?v=nYbfxeSGKFM&ab_channel=SalesforceSupport"
            ),
        ),
    ]
    sf_organization_id: Annotated[
        str,
        Meta(
            description=(
                "See below for instructions: "
                "How to find your organization id "
                " https://help.salesforce.com/s/articleView?id=000385215&type=1"
            ),
        ),
    ]


class ReadProfilesParameters(Struct):
    last_modified_date: Annotated[
        t.Optional[str],
        Meta(
            description="Last modified date",
        ),
    ] = None
    limit: Annotated[
        int,
        Meta(
            description=(
                "Total number of items to pull from Salesforce."
                "By default limiting to {}".format(DEFAULT_LIMIT_PROFILES)
            ),
        ),
    ] = DEFAULT_LIMIT_PROFILES


class WriteProfilesParameters(Struct):
    pass


class ReadJobsParameters(Struct):
    last_modified_date: Annotated[
        t.Optional[str],
        Meta(
            description="Last modified date",
        ),
    ] = None
    limit: Annotated[
        int,
        Meta(
            description=(
                "Total number of items to pull from Salesforce."
                "By default limiting to {}".format(DEFAULT_LIMIT_JOBS)
            ),
        ),
    ] = DEFAULT_LIMIT_JOBS


def generic_read_factory(
    soql_query: str,
):
    def _read_items(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadProfilesParameters | ReadJobsParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[t.Dict]:
        sf = Salesforce(
            username=auth_parameters.sf_username,
            password=auth_parameters.sf_password,
            security_token=auth_parameters.sf_security_token,
            organizationId=auth_parameters.sf_organization_id,
        )
        if not incremental:
            if parameters.last_modified_date is None:
                raise Exception("last_modified_date cannot be None in ReadMode.sync")
            last_modified_date = parameters.last_modified_date
        else:
            if parameters.last_modified_date is not None:
                raise Exception(
                    "last_modified_date should not be supplied in ReadMode.incremental"
                )
            if incremental_token:
                try:
                    read_data = json.loads(incremental_token)
                    last_modified_date = read_data["last_modified_date"]
                    last_id = read_data["hrflow_id"]
                except json.JSONDecodeError as e:
                    raise Exception(
                        "Failed to JSON parse"
                        f" incremental_token={incremental_token} error={e}"
                    )
                except KeyError:
                    raise Exception("Failed to find expected key in incremental_token")
            else:
                last_modified_date = (
                    datetime.now(tz=timezone.utc)
                    .replace(year=2020)
                    .replace(hour=0, minute=0, second=0, microsecond=0)
                    .isoformat()
                )
                last_id = 0

        remaining = parameters.limit
        query = soql_query.format(
            last_modified_date=last_modified_date,
            limit_placeholder=(
                "LIMIT {}".format(remaining + 5)
                if remaining + 5 < SOQL_MAX_RETURNED_ROWS
                else ""
            ),
        )
        for item in sf.query_all_iter(query):
            if remaining <= 0:
                break
            if (
                item["LastModifiedDate"] == last_modified_date
                and item["Id__c"] <= last_id
            ):
                # This is why limit_placeholder is set with remainig + 5
                # In order to account for first items that might be already pulled
                continue
            yield item
            remaining -= 1

    return _read_items


def delete_from_salesforce(data: t.List[t.Tuple[t.Any, t.List[str]]]) -> None:
    for sf_bulk_type, sf_ids in data:
        if len(sf_ids) > 0:
            sf_bulk_type.delete(data=[dict(Id=sf_id) for sf_id in sf_ids])


# the "type ignore" are needed to suppress errors raised by Pyright type hinter
#  due to the dynamic nature of the simple_salesforce library.


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    sf = Salesforce(
        username=auth_parameters.sf_username,
        password=auth_parameters.sf_password,
        security_token=auth_parameters.sf_security_token,
        organizationId=auth_parameters.sf_organization_id,
    )
    for profile in items:
        experiences, experience_sf_ids = (
            profile.pop("HrFlow_Profile_Experiences__r", None),
            [],
        )
        educations, education_sf_ids = (
            profile.pop("HrFlow_Profile_Educations__r", None),
            [],
        )
        attachments, attachment_sf_ids = (
            profile.pop("HrFlow_Profile_Attachments__r", None),
            [],
        )

        try:
            sf_profile_id = sf.HrFlow_Profile__c.create(profile)["id"]  # type: ignore
        except SalesforceError as e:
            adapter.error(
                "Failed to push profile(reference={}, id={}) to HrFlow_Profile__c with"
                " error={}".format(profile["Reference__c"], profile["Id__c"], e.content)
            )
            failed.append(profile)
            continue

        try:
            for related, sf_type, record_ids in [
                (experiences, sf.HrFlow_Experience__c, experience_sf_ids),
                (educations, sf.HrFlow_Education__c, education_sf_ids),
                (attachments, sf.HrFlow_Attachment__c, attachment_sf_ids),
            ]:
                records = related["records"] if related else []
                for i, record in enumerate(records):
                    record["Profile__c"] = sf_profile_id
                    record_ids.append(sf_type.create(record)["id"])  # type: ignore
        except SalesforceError as e:
            adapter.error(
                "Failed to push related {} number={} for profile(reference={}, id={},"
                " sf_id={}) data={} with error={}. Cleaning profile and related from"
                " Salesforce and adding as failed item".format(
                    e.resource_name,
                    i,
                    profile["Reference__c"],
                    profile["Id__c"],
                    sf_profile_id,
                    record,
                    e.content,
                )
            )
            delete_from_salesforce(
                data=[
                    (sf.bulk.HrFlow_Profile__c, [sf_profile_id]),  # type: ignore
                    (sf.bulk.HrFlow_Experience__c, experience_sf_ids),  # type: ignore
                    (sf.bulk.HrFlow_Education__c, education_sf_ids),  # type: ignore
                    (sf.bulk.HrFlow_Attachment__c, attachment_sf_ids),  # type: ignore
                ]
            )
            failed.append(profile)
            continue

    return failed


def update_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    sf = Salesforce(
        username=auth_parameters.sf_username,
        password=auth_parameters.sf_password,
        security_token=auth_parameters.sf_security_token,
        organizationId=auth_parameters.sf_organization_id,
    )

    for profile in items:
        experiences = profile.pop("HrFlow_Profile_Experiences__r", None)
        educations = profile.pop("HrFlow_Profile_Educations__r", None)
        attachments = profile.pop("HrFlow_Profile_Attachments__r", None)

        try:
            # Look for existing profile by Reference__c
            query_result = sf.query(
                "SELECT Id FROM HrFlow_Profile__c WHERE Reference__c ="
                f" '{profile['Reference__c']}'"
            )
            if not query_result["records"]:
                adapter.error(
                    "No matching profile found for"
                    f" Reference__c={profile['Reference__c']}."
                )
                failed.append(profile)
                continue

            # Update the main profile
            sf_profile_id = query_result["records"][0]["Id"]
            sf.HrFlow_Profile__c.update(sf_profile_id, profile)  # type: ignore

            # Handle related data
            for related_data, sf_type, identifier_field in [
                (experiences, sf.HrFlow_Experience__c, "Title__c"),
                (educations, sf.HrFlow_Education__c, "Title__c"),
                (attachments, sf.HrFlow_Attachment__c, "File_Name__c"),
            ]:
                if not related_data or not related_data.get("records"):
                    continue

                # Query existing related records
                existing_records = sf.query(
                    f"SELECT Id, {identifier_field} FROM {sf_type.name} WHERE"
                    f" Profile__c = '{sf_profile_id}'"
                )["records"]

                # Map existing records by identifier_field
                existing_map = {r[identifier_field]: r["Id"] for r in existing_records}

                # Determine records to update, create, and delete
                new_map = {r[identifier_field]: r for r in related_data["records"]}
                to_update = [
                    (existing_map[identifier], new_map[identifier])
                    for identifier in new_map
                    if identifier in existing_map
                ]
                to_create = [
                    record
                    for identifier, record in new_map.items()
                    if identifier not in existing_map
                ]
                to_delete = [
                    existing_map[identifier]
                    for identifier in existing_map
                    if identifier not in new_map
                ]

                # Perform CRUD operations
                for record_id, data in to_update:
                    sf_type.update(record_id, data)  # type: ignore
                for data in to_create:
                    data["Profile__c"] = sf_profile_id
                    sf_type.create(data)  # type: ignore
                for record_id in to_delete:
                    sf_type.delete(record_id)  # type: ignore

        except SalesforceError as e:
            adapter.error(
                f"Failed to update profile(reference={profile['Reference__c']}) with"
                f" error={e.content}"
            )
            failed.append(profile)
            continue

    return failed


def archive_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    sf = Salesforce(
        username=auth_parameters.sf_username,
        password=auth_parameters.sf_password,
        security_token=auth_parameters.sf_security_token,
        organizationId=auth_parameters.sf_organization_id,
    )

    for profile in items:
        try:
            # Look for existing profile by Reference__c
            query_result = sf.query(
                "SELECT Id FROM HrFlow_Profile__c WHERE Reference__c ="
                f" '{profile['Reference__c']}'"
            )
            if not query_result["records"]:
                adapter.error(
                    "No matching profile found for"
                    f" Reference__c={profile['Reference__c']}."
                )
                failed.append(profile)
                continue

            sf_profile_id = query_result["records"][0]["Id"]

            # Handle deletion of related data
            for sf_type, related_name in [
                (sf.HrFlow_Experience__c, "HrFlow_Experience__c"),
                (sf.HrFlow_Education__c, "HrFlow_Education__c"),
                (sf.HrFlow_Attachment__c, "HrFlow_Attachment__c"),
            ]:
                try:
                    related_records = sf.query(
                        f"SELECT Id FROM {related_name} WHERE Profile__c ="
                        f" '{sf_profile_id}'"
                    )["records"]
                    related_ids = [record["Id"] for record in related_records]
                    if related_ids:
                        sf_type.delete(related_ids)  # type: ignore
                except SalesforceError as e:
                    adapter.error(
                        f"Failed to delete related data from {related_name} for profile"
                        f" Reference__c={profile['Reference__c']} with"
                        f" error={e.content}"
                    )
                    raise

            # Delete the main profile
            try:
                sf.HrFlow_Profile__c.delete(sf_profile_id)  # type: ignore
            except SalesforceError as e:
                adapter.error(
                    "Failed to delete profile"
                    f" Reference__c={profile['Reference__c']} with error={e.content}"
                )
                raise

        except SalesforceError as e:
            adapter.error(
                f"Failed to archive profile(reference={profile['Reference__c']}) with"
                f" error={e.content}"
            )
            failed.append(profile)
            continue

    return failed


def item_to_read_from(item: t.Dict) -> str:
    return json.dumps(
        dict(
            last_modified_date=item["LastModifiedDate"],
            hrflow_id=item["Id__c"],
        )
    )


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=SalesforceHrFlowProfile,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=generic_read_factory(soql_query=SELECT_PROFILES_SOQL),
            update=generic_read_factory(soql_query=SELECT_PROFILES_SOQL),
            archive=generic_read_factory(soql_query=SELECT_PROFILES_SOQL),
        ),
        incremental_token_handler=IncrementalTokenHandler(
            create=item_to_read_from,
            update=item_to_read_from,
            archive=item_to_read_from,
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
            update=update_profiles,
            archive=archive_profiles,
        ),
    ),
)

JobsAisle = Aisle(
    name=Entity.job,
    schema=SalesforceHrFlowJob,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=generic_read_factory(soql_query=SELECT_JOBS_SOQL),
            update=generic_read_factory(soql_query=SELECT_JOBS_SOQL),
            archive=generic_read_factory(soql_query=SELECT_JOBS_SOQL),
        ),
        incremental_token_handler=IncrementalTokenHandler(
            create=item_to_read_from,
            update=item_to_read_from,
            archive=item_to_read_from,
        ),
    ),
)
