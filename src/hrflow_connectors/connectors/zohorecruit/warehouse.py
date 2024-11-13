import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.zohorecruit.schemas import Candidate, JobOpening
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

AUTHORIZATION_URL = "{zoho_accounts_url}/oauth/v2/token"
RECORDS_URL = "{recruit_accounts_url}/recruit/v2/{module_api_name}"


DESC = (
    "Pre requisites: a Zoho Recruit account and a registered client.\nTo register go"
    " to Zoho Developer Console: https://api-console.zoho.com/ and choose the Self"
    " Client type.\nthe creation of the Self Client will provide you with a Client ID"
    " and a Client Secret.\nYou need to then click on the 'Generate Code' button and"
    " porvide the required scopes (ZohoRecruit.modules.ALL for access to all modules in"
    " Recruite) to get the Authorization Code(grant token).\nfor  more information"
    " check the following links:\n"
    "https://www.zoho.com/recruit/developer-guide/apiv2/auth-request.html#:~:text=scopes%20here.-,Self%2DClient%20option,-Use%20this%20method \n"
    "https://help.zoho.com/portal/en/kb/recruit/developer-guide/oauth-authentication/overview/articles/oauth-overview#Scopes:~:text=the%20end%20user.-,Scopes%C2%A0%C2%A0,-Zoho%20Recruit%20APIs"
)


class ZohoAccountsURL(str, Enum):
    US = "https://accounts.zoho.com"
    AU = "https://accounts.zoho.com.au"
    EU = "https://accounts.zoho.eu"
    IN = "https://accounts.zoho.in"
    CN = "https://accounts.zoho.com.cn"
    JP = "https://accounts.zoho.jp"
    CA = "https://accounts.zohocloud.ca"


class RecruitAccountsURL(str, Enum):
    US = "https://recruit.zoho.com"
    EU = "https://recruit.zoho.eu"
    IN = "https://recruit.zoho.in"
    CN = "https://recruit.zoho.com.cn"
    AU = "https://recruit.zoho.com.au"
    JP = "https://recruit.zoho.jp"
    CA = "https://recruit.zohocloud.ca"


# Mapping from ZohoAccountsURL to RecruitAccountsURL
url_mapping = {
    ZohoAccountsURL.US: RecruitAccountsURL.US,
    ZohoAccountsURL.AU: RecruitAccountsURL.AU,
    ZohoAccountsURL.EU: RecruitAccountsURL.EU,
    ZohoAccountsURL.IN: RecruitAccountsURL.IN,
    ZohoAccountsURL.CN: RecruitAccountsURL.CN,
    ZohoAccountsURL.JP: RecruitAccountsURL.JP,
    ZohoAccountsURL.CA: RecruitAccountsURL.CA,
}


class ZohoAuthenticationParameters(ParametersModel):
    zoho_accounts_url: ZohoAccountsURL = Field(
        ZohoAccountsURL.EU,
        description=(
            "Zoho CRM is hosted at multiple data centers. Therefore, the API domain URL"
            " varies for each data center.\nYou must use your domain-specific Zoho"
            " Accounts URL to generate access and refresh tokens. The following are the"
            " various domains and their corresponding accounts URLs.\n US:"
            " https://accounts.zoho.com\n AU: https://accounts.zoho.com.au\n EU:"
            " https://accounts.zoho.eu\n IN: https://accounts.zoho.in\n CN:"
            " https://accounts.zoho.com.cn\n JP: https://accounts.zoho.jp\n CA:"
            " https://accounts.zohocloud.ca"
        ),
        field_type=FieldType.Auth,
    )
    client_id: str = Field(
        ...,
        description="A unique ID displayed under Self Client > Client Secret.",
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        description=(
            "A unique confidential secret displayed under Self Client > Client Secret."
        ),
        field_type=FieldType.Auth,
    )
    authorization_code: str = Field(
        ...,
        description=(
            "The authorization code generated during the Self Client creation, used to"
            " get the refresh token and the first access token."
        ),
        field_type=FieldType.Auth,
    )
    refresh_token: t.Optional[str] = Field(
        None,
        description=(
            "The refresh token is used to generate a new access token when the current"
            " access token expires."
        ),
        field_type=FieldType.Auth,
    )


class SortOrder(str, Enum):
    ASC = "asc"
    DESC = "desc"


class ZohoBool(str, Enum):
    TRUE = "true"
    FALSE = "false"
    BOTH = "both"


class State(str, Enum):
    DRAFT = "draft"
    SAVE = "save"


class ReadingParameters(ParametersModel):
    fields: t.Optional[str] = Field(
        None,
        description=(
            "To list all the module records with respect to fields\nMultiple field API"
            " names, comma-separated.\nFor example Last_Name, Email"
        ),
        field_type=FieldType.QueryParam,
    )
    sort_order: t.Optional[SortOrder] = Field(
        None,
        description=(
            "To sort the available list of records in either ascending or descending"
            " order\nasc - ascending order\ndesc - descending order"
        ),
        field_type=FieldType.QueryParam,
    )
    sort_by: t.Optional[str] = Field(
        None,
        description=(
            "To sort the available list of records based on the given field\nField API"
            " name\nExample: Email"
        ),
        field_type=FieldType.QueryParam,
    )
    converted: t.Optional[ZohoBool] = Field(
        ZohoBool.FALSE,
        description=(
            "To get the list of converted records.\nThe default value is false\ntrue -"
            " get only converted records\nfalse - get only non-converted records\nboth"
            " - get all records"
        ),
        field_type=FieldType.QueryParam,
    )
    approved: t.Optional[ZohoBool] = Field(
        ZohoBool.TRUE,
        description=(
            "To get the list of approved records.\nThe default value is true\ntrue -"
            " get only approved records\nfalse - get only non-approved records\nboth -"
            " get all records"
        ),
        field_type=FieldType.QueryParam,
    )
    cvid: t.Optional[int] = Field(
        None,
        description=(
            "To get the list of records based on custom views\n{custom_view_id}"
        ),
        field_type=FieldType.QueryParam,
    )
    territory_id: t.Optional[int] = Field(
        None,
        description="To get the list of records based on territory\n{territory_id}",
        field_type=FieldType.QueryParam,
    )
    include_child: t.Optional[bool] = Field(
        None,
        description=(
            "To include records from the child territories.\nTrue includes child"
            " territory records.\nFalse does not include child territory records.\nThe"
            " default value is false."
        ),
        field_type=FieldType.QueryParam,
    )
    state: t.Optional[State] = Field(
        None,
        description=(
            "If the value of this parameter is 'draft', then the response will only"
            " contain Draft records from the specified module. If the parameter's value"
            " is 'save', then the response will return saved records from the specified"
            " module.\n\nIf this parameter is not included in your request body, then"
            " the response will only return saved records from the specified module."
        ),
        field_type=FieldType.QueryParam,
    )


class ReadParameters(ZohoAuthenticationParameters, ReadingParameters):
    pass


class WriteParameters(ZohoAuthenticationParameters):
    pass


class UpdateParameters(ZohoAuthenticationParameters):
    pass


class DeleteParameters(ZohoAuthenticationParameters):
    pass


def get_refresh_token_and_first_access_token(
    adapter, authorization_url, client_id, client_secret, authorization_code
):
    request_data = {
        "client_id": client_id,
        "client_secret": client_secret,
        "grant_type": "authorization_code",
        "code": authorization_code,
    }

    response = requests.post(authorization_url, data=request_data)
    if response.status_code == 200:
        response = response.json()
        if "error" in response:
            if response["error"] == "invalid_code":
                adapter.info(
                    "The grant token has expired or  have already been used, use"
                    " the refresh token from the previous response to get a new"
                    " access token"
                )
                return None, None

            elif response["error"] == "invalid_client":
                adapter.error("The client ID or client secret is invalid")

    access_token = response["access_token"]
    refresh_token = response["refresh_token"]
    return access_token, refresh_token


def refresh_access_token(
    adapter,
    authorization_url,
    client_id,
    client_secret,
    refresh_token,
):
    request_data = {
        "client_id": client_id,
        "client_secret": client_secret,
        "grant_type": "refresh_token",
        "refresh_token": refresh_token,
    }

    response = requests.post(authorization_url, data=request_data)
    if response.status_code == 200:
        response_data = response.json()
        if "error" in response_data:
            if response_data["error"] == "invalid_code":
                adapter.error(
                    "The refresh token to generate a new access token is wrong or"
                    " revoked."
                )
            elif response_data["error"] == "invalid_client":
                adapter.error("The client ID or client secret is invalid")
        return response["access_token"]

    else:
        raise Exception(
            f"Failed to get access token: {response.text}, response status code"
            f" {response.status_code}"
        )


def generic_read(
    module_api_name: str,
) -> t.Callable[
    [
        LoggerAdapter,
        ReadParameters,
        t.Optional[ReadMode],
        t.Optional[str],
    ],
    t.Iterable[t.Dict],
]:
    def __pull_records(
        adapter: LoggerAdapter,
        parameters: ReadParameters,
        read_mode: t.Optional[ReadMode] = None,
        read_from: t.Optional[str] = None,
    ) -> t.Iterable[t.Dict]:
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[parameters.zoho_accounts_url]
        url = RECORDS_URL.format(
            recruit_accounts_url=recruit_accounts_url, module_api_name=module_api_name
        )

        if parameters.refresh_token:
            refresh_token = parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                parameters.client_id,
                parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                parameters.client_id,
                parameters.client_secret,
                parameters.authorization_code,
            )

        params = parameters.dict(
            exclude={
                "zoho_accounts_url",
                "client_id",
                "client_secret",
                "authorization_code",
                "refresh_token",
                "state",
            }
        )
        if parameters.state:
            params["$state"] = parameters.state

        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        while True:
            response = requests.get(url, headers=headers, params=params)
            if response.status_code == 200:
                response = response.json()
                data = response["data"]
                for record in data:
                    full_record_response = requests.get(
                        f"{url}/{record['id']}", headers=headers
                    )
                    if full_record_response.status_code == 200:
                        yield full_record_response.json()["data"][0]
                    else:
                        adapter.error(
                            f"Failed to fetch record: {full_record_response.text},"
                            f" response status code {full_record_response.status_code}"
                        )
                if not response["info"]["more_records"]:
                    break
                params["page"] = response["info"]["page"] + 1
            elif response.status_code == 401:
                access_token = refresh_access_token(
                    adapter,
                    authorization_url,
                    parameters.client_id,
                    parameters.client_secret,
                    refresh_token,
                )
                if not access_token:
                    raise Exception("Failed to refresh access token")
                headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
            else:
                adapter.error(
                    f"Failed to fetch records: {response.text}, response status code"
                    f" {response.status_code}"
                )
                break

    return __pull_records


def generic_write(
    module_api_name: str,
) -> t.Callable[[LoggerAdapter, WriteParameters, t.Iterable[t.Dict]], t.List[t.Dict]]:
    def __push_records(
        adapter: LoggerAdapter,
        parameters: ZohoAuthenticationParameters,
        records: t.Iterable[t.Dict],
    ) -> t.List[t.Dict]:
        failed_records = []
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[parameters.zoho_accounts_url]
        url = RECORDS_URL.format(
            recruit_accounts_url=recruit_accounts_url, module_api_name=module_api_name
        )

        if parameters.refresh_token:
            refresh_token = parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                parameters.client_id,
                parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                parameters.client_id,
                parameters.client_secret,
                parameters.authorization_code,
            )

        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}

        for record in records:
            _input = {"data": [record]}
            response = requests.post(url, headers=headers, json=_input)
            if response.status_code == 201:
                id = response.json()["data"][0]["details"]["id"]
                adapter.info(f"Record {id} created")
            elif response.status_code == 401:
                access_token = refresh_access_token(
                    adapter,
                    authorization_url,
                    parameters.client_id,
                    parameters.client_secret,
                    refresh_token,
                )
                if not access_token:
                    raise Exception("Failed to refresh access token")
                headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
                response = requests.post(url, headers=headers, json=_input)
                if response.status_code == 200:
                    adapter.info(f"Record {id} created")
                else:
                    adapter.error(
                        f"Failed to create record: {response.text}, response status"
                        f" code {response.status_code}"
                    )
                    failed_records.append(record)
            else:
                adapter.error(
                    f"Failed to create record: {response.text}, response status code"
                    f" {response.status_code}"
                )
                failed_records.append(record)
        return failed_records

    return __push_records


def generic_update(
    module_api_name: str,
) -> t.Callable[
    [
        LoggerAdapter,
        UpdateParameters,
        t.Iterable[t.Dict],
    ],
    t.List[t.Dict],
]:
    def __update_records(
        adapter: LoggerAdapter,
        parameters: UpdateParameters,
        records: t.Iterable[t.Dict],
    ) -> t.List[t.Dict]:
        failed_records = []
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[parameters.zoho_accounts_url]
        url = RECORDS_URL.format(
            recruit_accounts_url=recruit_accounts_url, module_api_name=module_api_name
        )

        if parameters.refresh_token:
            refresh_token = parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                parameters.client_id,
                parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                parameters.client_id,
                parameters.client_secret,
                parameters.authorization_code,
            )

        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        for record in records:
            _input = {"data": [record]}
            response = requests.put(url, headers=headers, json=_input)
            if response.status_code == 200:
                id = response.json()["data"][0]["details"]["id"]
                adapter.info(f"Record {id} updated")
            elif response.status_code == 401:
                access_token = refresh_access_token(
                    adapter,
                    authorization_url,
                    parameters.client_id,
                    parameters.client_secret,
                    refresh_token,
                )
                if not access_token:
                    raise Exception("Failed to refresh access token")
                headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
                response = requests.put(url, headers=headers, json=_input)
                if response.status_code == 200:
                    adapter.info(f"Record {id} updated")
                else:
                    adapter.error(
                        f"Failed to update record: {response.text}, response status"
                        f" code {response.status_code}"
                    )
                    failed_records.append(record)
            else:
                adapter.error(
                    f"Failed to update record: {response.text}, response status code"
                    f" {response.status_code}"
                )
                failed_records.append(record)
        return failed_records

    return __update_records


def generic_delete(
    module_api_name: str,
) -> t.Callable[
    [
        LoggerAdapter,
        DeleteParameters,
        t.Iterable[t.Dict],
    ],
    t.List[t.Dict],
]:
    def __delete_records(
        adapter: LoggerAdapter,
        parameters: DeleteParameters,
        records: t.Iterable[t.Dict],
    ) -> t.List[t.Dict]:
        failed_records = []
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[parameters.zoho_accounts_url]
        url = RECORDS_URL.format(
            recruit_accounts_url=recruit_accounts_url, module_api_name=module_api_name
        )

        if parameters.refresh_token:
            refresh_token = parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                parameters.client_id,
                parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                parameters.client_id,
                parameters.client_secret,
                parameters.authorization_code,
            )

        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        for record in records:
            response = requests.delete(f"{url}?ids={record['id']}", headers=headers)
            if response.status_code == 200:
                id = response.json()["data"][0]["details"]["id"]
                adapter.info(f"Record {id} deleted")
            elif response.status_code == 401:
                access_token = refresh_access_token(
                    adapter,
                    authorization_url,
                    parameters.client_id,
                    parameters.client_secret,
                    refresh_token,
                )
                if not access_token:
                    raise Exception("Failed to refresh access token")
                headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
                response = requests.delete(f"{url}?ids={record['id']}", headers=headers)
                if response.status_code == 200:
                    adapter.info(f"Record {id} deleted")
                else:
                    adapter.error(
                        f"Failed to delete record: {response.text}, response status"
                        f" code {response.status_code}"
                    )
                    failed_records.append(record)
            else:
                adapter.error(
                    f"Failed to delete record: {response.text}, response status code"
                    f" {response.status_code}"
                )
                failed_records.append(record)
        return failed_records

    return __delete_records


ZohoRecruitJobWarehouse = Warehouse(
    name="Zoho Jobs",
    data_schema=JobOpening,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadParameters,
        function=generic_read("JobOpenings"),
    ),
    write=WarehouseWriteAction(
        parameters=WriteParameters,
        function=generic_write("JobOpenings"),
    ),
)
ZohoRecruitCandidatesWarehouse = Warehouse(
    name="Zoho Candidates",
    data_schema=Candidate,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadParameters,
        function=generic_read("Candidates"),
    ),
    write=WarehouseWriteAction(
        parameters=WriteParameters,
        function=generic_write("Candidates"),
    ),
)
