import json
import typing as t
from datetime import datetime, timezone
from logging import LoggerAdapter

from pydantic import Field
from simple_salesforce import Salesforce

from hrflow_connectors.connectors.salesforce.schemas import SalesforceHrFlowProfile
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
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
MAX_SOQL_LIMIT = 200
MAX_PROFILES = 500


class ReadProfilesParameters(ParametersModel):
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
    last_modified_date: t.Optional[str] = Field(
        None,
        description="Last modified date",
        field_type=FieldType.QueryParam,
    )
    limit: t.Optional[int] = Field(
        MAX_PROFILES,
        description=(
            "Number of profiles to pull from Salesforce. Maximum value is {}".format(
                MAX_PROFILES
            )
        ),
        field_type=FieldType.QueryParam,
    )


def read(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
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
                raise Exception(f"Failed to JSON parse read_from={read_from} error={e}")
            except KeyError as e:
                raise Exception(
                    "Failed to find expected key in"
                    f" read_from={read_from} error={repr(e)}"
                )
        else:
            last_modified_date = (
                datetime.now(tz=timezone.utc)
                .replace(hour=0, minute=0, second=0, microsecond=0)
                .isoformat()
            )
            last_id = 0

    offset = 0
    profiles_to_pull = min(MAX_PROFILES, parameters.limit)
    limit = min(MAX_SOQL_LIMIT, profiles_to_pull)

    while profiles_to_pull > 0:
        query = SELECT_PROFILES_SOQL.format(
            last_modified_date=last_modified_date,
            limit=limit,
            offset=offset,
        )
        profiles = sf.query_all(query)["records"]
        if len(profiles) == 0:
            break
        new_profiles = 0
        for profile in profiles:
            if (
                profile["LastModifiedDate"] == last_modified_date
                and profile["Id__c"] <= last_id
            ):
                continue
            new_profiles += 1
            yield profile

        offset += len(profiles)
        profiles_to_pull -= new_profiles
        limit = min(MAX_SOQL_LIMIT, profiles_to_pull)


SalesforceProfileWarehouse = Warehouse(
    name="Salesforce Profiles",
    data_schema=SalesforceHrFlowProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read,
        supports_incremental=True,
        item_to_read_from=lambda profile: json.dumps(
            dict(
                last_modified_date=profile["LastModifiedDate"],
                hrflow_id=profile["Id__c"],
            )
        ),
    ),
)
