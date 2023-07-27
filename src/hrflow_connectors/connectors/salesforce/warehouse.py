import typing as t
from logging import LoggerAdapter
from datetime import date

from pydantic import Field
from simple_salesforce import Salesforce



from hrflow_connectors.connectors.salesforce.schemas import (
    SalesforceHrFlowProfile,
)
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
LIMIT {limit}
OFFSET {offset}
"""
MAX_SOQL_LIMIT = 200
MAX_PROFILES = 1000


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
        description="Security Token to access Salesforce API." 
            "See below for instructions: "
            "How Can I Find My Security Token and Use It in Data Loader | Salesforce Platform "
            " https://www.youtube.com/watch?v=nYbfxeSGKFM&ab_channel=SalesforceSupport",
        repr=False,
        field_type=FieldType.Auth,
    )
    sf_organization_id: str = Field(
        ...,
        description="Security Token to access Salesforce API." 
            "See below for instructions: "
            "How to find your organization id "
            " https://help.salesforce.com/s/articleView?id=000385215&type=1",
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
        organizationId=parameters.sf_organization_id
    )
    if read_mode is ReadMode.sync:
        if parameters.last_modified_date is None:
            raise Exception("last_modified_date cannot be None in ReadMode.sync")
        last_modified_date = parameters.last_modified_date
    else:
        last_modified_date = read_from or date.today().isoformat()
    

    offset = 0
    profiles_to_pull = min(MAX_PROFILES, parameters.limit)
    limit = min(MAX_SOQL_LIMIT, profiles_to_pull)

    while profiles_to_pull > 0:
        query = SELECT_PROFILES_SOQL.format(
            last_modified_date=last_modified_date,
            limit=limit,
            offset=offset
        )
        profiles = sf.query_all(query)["records"]
        for profile in profiles:
            yield profile

        offset += len(profiles)
        profiles_to_pull -= offset
        limit = min(MAX_SOQL_LIMIT, profiles_to_pull)


SalesforceProfileWarehouse = Warehouse(
    name="Salesforce Profiles",
    data_schema=SalesforceHrFlowProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read,
    ),
)