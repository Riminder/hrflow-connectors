import typing as t
from enum import Enum
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from msgspec.structs import asdict
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.zohorecruit.schemas import Candidate, JobOpening
from hrflow_connectors.v2.core.common import Entity, Mode
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    ReadOperation,
    WriteOperation,
    merge,
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


class AuthParameters(Struct):
    client_id: Annotated[
        str,
        Meta(
            description="A unique ID displayed under Self Client > Client Secret.",
        ),
    ]
    client_secret: Annotated[
        str,
        Meta(
            description=(
                "A unique confidential secret displayed under Self Client > Client"
                " Secret."
            ),
        ),
    ]
    authorization_code: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The authorization code generated during the Self Client creation, used"
                " to get the refresh token and the first access token."
            ),
        ),
    ] = None
    refresh_token: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "The refresh token is used to generate a new access token when the"
                " current access token expires."
            ),
        ),
    ] = None
    zoho_accounts_url: Annotated[
        ZohoAccountsURL,
        Meta(
            description=(
                "Zoho CRM is hosted at multiple data centers. Therefore, the API domain"
                " URL varies for each data center.\nYou must use your domain-specific"
                " Zoho Accounts URL to generate access and refresh tokens. The"
                " following are the various domains and their corresponding accounts"
                " URLs.\n US: https://accounts.zoho.com\n AU:"
                " https://accounts.zoho.com.au\n EU: https://accounts.zoho.eu\n IN:"
                " https://accounts.zoho.in\n CN: https://accounts.zoho.com.cn\n JP:"
                " https://accounts.zoho.jp\n CA: https://accounts.zohocloud.ca"
            ),
        ),
    ] = ZohoAccountsURL.EU


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


class Type(str, Enum):
    ALL = "all"
    RECYCLE = "recycle"
    PERMANENT = "permanent"


class ReadParameters(Struct):
    fields: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "To list all the module records with respect to fields\nMultiple field"
                " API names, comma-separated.\nFor example Last_Name, Email"
            ),
        ),
    ] = None
    sort_order: Annotated[
        t.Optional[SortOrder],
        Meta(
            description=(
                "To sort the available list of records in either ascending or"
                " descending order\nasc - ascending order\ndesc - descending order"
            ),
        ),
    ] = None
    sort_by: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "To sort the available list of records based on the given field\nField"
                " API name\nExample: Email"
            ),
        ),
    ] = None
    cvid: Annotated[
        t.Optional[int],
        Meta(
            description=(
                "To get the list of records based on custom views\n{custom_view_id}"
            ),
        ),
    ] = None
    territory_id: Annotated[
        t.Optional[int],
        Meta(
            description="To get the list of records based on territory\n{territory_id}",
        ),
    ] = None
    include_child: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "To include records from the child territories.\nTrue includes child"
                " territory records.\nFalse does not include child territory"
                " records.\nThe default value is false."
            ),
        ),
    ] = None
    state: Annotated[
        t.Optional[State],
        Meta(
            description=(
                "If the value of this parameter is 'draft', then the response will only"
                " contain Draft records from the specified module. If the parameter's"
                " value is 'save', then the response will return saved records from the"
                " specified module.\n\nIf this parameter is not included in your"
                " request body, then the response will only return saved records from"
                " the specified module."
            ),
        ),
    ] = None
    converted: Annotated[
        t.Optional[ZohoBool],
        Meta(
            description=(
                "To get the list of converted records.\nThe default value is"
                " false\ntrue - get only converted records\nfalse - get only"
                " non-converted records\nboth - get all records"
            ),
        ),
    ] = ZohoBool.FALSE
    approved: Annotated[
        t.Optional[ZohoBool],
        Meta(
            description=(
                "To get the list of approved records.\nThe default value is true\ntrue"
                " - get only approved records\nfalse - get only non-approved"
                " records\nboth - get all records"
            ),
        ),
    ] = ZohoBool.TRUE


class ReadDeleteParameters(Struct):
    type: Annotated[
        t.Optional[Type],
        Meta(
            description=(
                "All\nTo get the list of all deleted records\nRecycle\nTo get the list"
                " of deleted records from recycle bin\nPermanent\nTo get the list of"
                " permanently deleted records"
            ),
        ),
    ] = Type.ALL


class WriteParameters(Struct):
    pass


class WriteDeleteParameters(Struct):
    wf_trigger: Annotated[
        t.Optional[bool],
        Meta(
            description=(
                "Represents if the workflow rules are to be triggered upon record"
                " deletion. The value true triggers workflows. The value false does not"
                " trigger workflows. The default value is true."
            ),
        ),
    ] = True


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
                return None, None

    access_token = response.json()["access_token"]
    refresh_token = response.json()["refresh_token"]
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
        return response_data["access_token"]

    else:
        raise Exception(
            f"Failed to get access token: {response.text}, response status code"
            f" {response.status_code}"
        )


def generic_read(
    action: Mode,
    module_api_name: str,
):
    def __pull_records(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[t.Dict]:
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=auth_parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[auth_parameters.zoho_accounts_url]
        url = RECORDS_URL.format(
            recruit_accounts_url=recruit_accounts_url, module_api_name=module_api_name
        )

        updated_date_field = (
            "Modified_Time" if module_api_name == "JobOpenings" else "Updated_On"
        )

        if auth_parameters.refresh_token:
            refresh_token = auth_parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                auth_parameters.authorization_code,
            )

        params = asdict(parameters)
        state = params.pop("state", None)
        if state:
            params["$state"] = state

        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        while True:
            response = requests.get(url, headers=headers, params=params)
            if response.status_code == 200:
                response = response.json()
                data = response["data"]
                for record in data:
                    if (
                        action == Mode.create
                        and record[updated_date_field] != record["Created_Time"]
                    ) or (
                        action == Mode.update
                        and record[updated_date_field] == record["Created_Time"]
                    ):
                        continue

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
                    auth_parameters.client_id,
                    auth_parameters.client_secret,
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


def generic_read_deleted(
    module_api_name: str,
):
    def __pull_deleted_records(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: ReadDeleteParameters,
        incremental: bool,
        incremental_token: t.Optional[str],
    ) -> t.Iterable[t.Dict]:
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=auth_parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[auth_parameters.zoho_accounts_url]
        url = (
            RECORDS_URL.format(
                recruit_accounts_url=recruit_accounts_url,
                module_api_name=module_api_name,
            )
            + "/deleted"
        )

        if auth_parameters.refresh_token:
            refresh_token = auth_parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                auth_parameters.authorization_code,
            )

        params = asdict(parameters)
        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        while True:
            response = requests.get(url, headers=headers, params=params)
            if response.status_code == 200:
                response = response.json()
                records = response["data"]
                for record in records:
                    yield record

                if not response["info"]["more_records"]:
                    break
                params["page"] = response["info"]["page"] + 1
            elif response.status_code == 204:
                adapter.info("No records found")
                break
            elif response.status_code == 401:
                access_token = refresh_access_token(
                    adapter,
                    authorization_url,
                    auth_parameters.client_id,
                    auth_parameters.client_secret,
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

    return __pull_deleted_records


def generic_write(
    module_api_name: str,
):
    def __push_records(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: WriteParameters,
        items: t.Iterable[t.Dict],
    ) -> t.List[t.Dict]:
        failed_records = []
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=auth_parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[auth_parameters.zoho_accounts_url]
        url = RECORDS_URL.format(
            recruit_accounts_url=recruit_accounts_url, module_api_name=module_api_name
        )

        if auth_parameters.refresh_token:
            refresh_token = auth_parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                auth_parameters.authorization_code,
            )

        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}

        for record in items:
            _input = {"data": [record]}
            response = requests.post(url, headers=headers, json=_input)
            if response.status_code == 201:
                id = response.json()["data"][0]["details"]["id"]
                adapter.info(f"Record {id} created")
            elif response.status_code == 401:
                access_token = refresh_access_token(
                    adapter,
                    authorization_url,
                    auth_parameters.client_id,
                    auth_parameters.client_secret,
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
):
    def __update_records(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: WriteParameters,
        items: t.Iterable[t.Dict],
    ) -> t.List[t.Dict]:
        failed_records = []
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=auth_parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[auth_parameters.zoho_accounts_url]
        url = RECORDS_URL.format(
            recruit_accounts_url=recruit_accounts_url, module_api_name=module_api_name
        )

        if auth_parameters.refresh_token:
            refresh_token = auth_parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                auth_parameters.authorization_code,
            )

        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        for record in items:
            _input = {"data": [record]}
            response = requests.put(url, headers=headers, json=_input)
            if response.status_code == 200:
                id = response.json()["data"][0]["details"]["id"]
                adapter.info(f"Record {id} updated")
            elif response.status_code == 401:
                access_token = refresh_access_token(
                    adapter,
                    authorization_url,
                    auth_parameters.client_id,
                    auth_parameters.client_secret,
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
):
    def __delete_records(
        adapter: LoggerAdapter,
        auth_parameters: AuthParameters,
        parameters: WriteDeleteParameters,
        items: t.Iterable[t.Dict],
    ) -> t.List[t.Dict]:
        failed_records = []
        authorization_url = AUTHORIZATION_URL.format(
            zoho_accounts_url=auth_parameters.zoho_accounts_url
        )
        recruit_accounts_url = url_mapping[auth_parameters.zoho_accounts_url]
        url = RECORDS_URL.format(
            recruit_accounts_url=recruit_accounts_url, module_api_name=module_api_name
        )

        if auth_parameters.refresh_token:
            refresh_token = auth_parameters.refresh_token
            access_token = refresh_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                refresh_token,
            )
        else:
            access_token, refresh_token = get_refresh_token_and_first_access_token(
                adapter,
                authorization_url,
                auth_parameters.client_id,
                auth_parameters.client_secret,
                auth_parameters.authorization_code,
            )

        headers = {"Authorization": f"Zoho-oauthtoken {access_token}"}
        params = asdict(parameters)
        for record in items:
            response = requests.delete(
                f"{url}?ids={record['id']}", headers=headers, params=params
            )
            if response.status_code == 200:
                id = response.json()["data"][0]["details"]["id"]
                adapter.info(f"Record {id} deleted")
            elif response.status_code == 401:
                access_token = refresh_access_token(
                    adapter,
                    authorization_url,
                    auth_parameters.client_id,
                    auth_parameters.client_secret,
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


JobsAisle = Aisle(
    name=Entity.job,
    schema=JobOpening,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadParameters,
            archive=ReadDeleteParameters,
            update=ReadParameters,
        ),
        function=merge(
            create=generic_read(module_api_name="JobOpenings", action=Mode.create),
            archive=generic_read_deleted("JobOpenings"),
            update=generic_read(module_api_name="JobOpenings", action=Mode.update),
        ),
    ),
)

ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=Candidate,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadParameters,
            archive=ReadDeleteParameters,
            update=ReadParameters,
        ),
        function=merge(
            create=generic_read(module_api_name="Candidates", action=Mode.create),
            archive=generic_read_deleted("Candidates"),
            update=generic_read(module_api_name="Candidates", action=Mode.update),
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteParameters,
            update=WriteParameters,
            archive=WriteDeleteParameters,
        ),
        function=merge(
            create=generic_write("Candidates"),
            update=generic_update("Candidates"),
            archive=generic_delete("Candidates"),
        ),
    ),
)
