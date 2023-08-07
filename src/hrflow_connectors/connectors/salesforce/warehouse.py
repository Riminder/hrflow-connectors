import json
import typing as t
from datetime import datetime, timezone
from logging import LoggerAdapter

from pydantic import Field
from simple_salesforce import Salesforce, SalesforceError
from simple_salesforce.bulk import SFBulkType

from hrflow_connectors.connectors.salesforce.schemas import (
    SalesforceHrFlowJob,
    SalesforceHrFlowProfile,
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

SELECT_PROFILES_SOQL = """
SELECT
    LastModifiedDate,
    FIELDS(CUSTOM),
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
LIMIT {limit}
OFFSET {offset}
"""

SELECT_JOBS_SOQL = """
SELECT
    LastModifiedDate,
    FIELDS(CUSTOM)
FROM HrFlow_Job__c
WHERE LastModifiedDate >= {last_modified_date}
ORDER BY LastModifiedDate, Id__c
LIMIT {limit}
OFFSET {offset}
"""
MAX_SOQL_LIMIT = 200
MAX_ITEMS = 500


class SalesforceBaseParameters(ParametersModel):
    sf_username: str = Field(
        ...,
        description="username used to access Salesforce API",
        repr=False,
        field_type=FieldType.Auth,
    )
    sf_password: str = Field(
        ...,
        description="password used to access Salesforce API",
        repr=False,
        field_type=FieldType.Auth,
    )
    sf_security_token: str = Field(
        ...,
        description=(
            "Security Token to access Salesforce API.See below for instructions: How"
            " Can I Find My Security Token and Use It in Data Loader | Salesforce"
            " Platform "
            " https://www.youtube.com/watch?v=nYbfxeSGKFM&ab_channel=SalesforceSupport"
        ),
        repr=False,
        field_type=FieldType.Auth,
    )
    sf_organization_id: str = Field(
        ...,
        description=(
            "Security Token to access Salesforce API."
            "See below for instructions: "
            "How to find your organization id "
            " https://help.salesforce.com/s/articleView?id=000385215&type=1"
        ),
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadFromSalesforceParameters(SalesforceBaseParameters):
    last_modified_date: t.Optional[str] = Field(
        None,
        description="Last modified date",
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        MAX_ITEMS,
        description=(
            "Number of items to pull from Salesforce. Maximum value is {}".format(
                MAX_ITEMS
            )
        ),
        field_type=FieldType.QueryParam,
    )


def generic_read_factory(
    soql_query: str,
) -> t.Callable[
    [LoggerAdapter, ParametersModel, t.Optional[ReadMode], t.Optional[str]],
    t.Iterable[t.Dict],
]:
    def _read_items(
        adapter: LoggerAdapter,
        parameters: ReadFromSalesforceParameters,
        read_mode: t.Optional[ReadMode] = None,
        read_from: t.Optional[str] = None,
    ) -> t.Iterable[t.Dict]:
        sf = Salesforce(
            username=parameters.sf_username,
            password=parameters.sf_password,
            security_token=parameters.sf_security_token,
            organizationId=parameters.sf_organization_id,
        )
        if read_mode is ReadMode.sync:
            if parameters.last_modified_date is None:
                raise Exception("last_modified_date cannot be None in ReadMode.sync")
            last_modified_date = parameters.last_modified_date
        else:
            if parameters.last_modified_date is not None:
                raise Exception(
                    "last_modified_date should not be supplied in ReadMode.incremental"
                )
            if read_from:
                try:
                    read_from = json.loads(read_from)
                    last_modified_date = read_from["last_modified_date"]
                    last_id = read_from["hrflow_id"]
                except json.JSONDecodeError as e:
                    raise Exception(
                        f"Failed to JSON parse read_from={read_from} error={e}"
                    )
                except KeyError as e:
                    raise Exception(
                        "Failed to find expected key in"
                        f" read_from={read_from} error={repr(e)}"
                    )
            else:
                last_modified_date = (
                    datetime.now(tz=timezone.utc)
                    .replace(year=2020)
                    .replace(hour=0, minute=0, second=0, microsecond=0)
                    .isoformat()
                )
                last_id = 0

        offset = 0
        items_to_pull = min(MAX_ITEMS, parameters.limit)
        limit = min(MAX_SOQL_LIMIT, items_to_pull)

        while items_to_pull > 0:
            query = soql_query.format(
                last_modified_date=last_modified_date,
                limit=limit,
                offset=offset,
            )
            items = sf.query_all(query)["records"]
            if len(items) == 0:
                break
            new_items = 0
            for item in items:
                if (
                    item["LastModifiedDate"] == last_modified_date
                    and item["Id__c"] <= last_id
                ):
                    continue
                new_items += 1
                yield item

            offset += len(items)
            items_to_pull -= new_items
            limit = min(MAX_SOQL_LIMIT, items_to_pull)

    return _read_items


def delete_from_salesforce(data: t.List[t.Tuple[SFBulkType, t.List[str]]]) -> None:
    for sf_bulk_type, sf_ids in data:
        if len(sf_ids) > 0:
            sf_bulk_type.delete(data=[dict(Id=sf_id) for sf_id in sf_ids])


def write_profiles(
    adapter: LoggerAdapter,
    parameters: ReadFromSalesforceParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed = []
    sf = Salesforce(
        username=parameters.sf_username,
        password=parameters.sf_password,
        security_token=parameters.sf_security_token,
        organizationId=parameters.sf_organization_id,
    )
    for profile in profiles:
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
            sf_profile_id = sf.HrFlow_Profile__c.create(profile)["id"]
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
                    record_ids.append(sf_type.create(record)["id"])
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
                    (sf.bulk.HrFlow_Profile__c, [sf_profile_id]),
                    (sf.bulk.HrFlow_Experience__c, experience_sf_ids),
                    (sf.bulk.HrFlow_Education__c, education_sf_ids),
                    (sf.bulk.HrFlow_Attachment__c, attachment_sf_ids),
                ]
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


SalesforceProfileWarehouse = Warehouse(
    name="Salesforce Profiles",
    data_schema=SalesforceHrFlowProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadFromSalesforceParameters,
        function=generic_read_factory(soql_query=SELECT_PROFILES_SOQL),
        supports_incremental=True,
        item_to_read_from=item_to_read_from,
    ),
    write=WarehouseWriteAction(
        parameters=SalesforceBaseParameters,
        function=write_profiles,
    ),
)

SalesforceJobWarehouse = Warehouse(
    name="Salesforce Jobs",
    data_schema=SalesforceHrFlowJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadFromSalesforceParameters,
        function=generic_read_factory(soql_query=SELECT_JOBS_SOQL),
        supports_incremental=True,
        item_to_read_from=item_to_read_from,
    ),
)
