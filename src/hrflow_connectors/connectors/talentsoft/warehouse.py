import json
import typing as t
from io import BytesIO
from logging import LoggerAdapter
from zipfile import ZipFile

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.core import Warehouse, WarehouseReadAction

GRANT_TYPE = "client_credentials"


class ReadProfileParameters(BaseModel):
    client_id: str = Field(
        ..., description="Client ID used to access TalentSoft API", repr=False
    )
    client_secret: str = Field(
        ..., description="Client Secret used to access TalentSoft API", repr=False
    )
    client_url: str = Field(..., description="URL of TalentSoft client integration")
    token_scope: str = Field(
        ..., description="Scope of the authentication token to request from TalentSoft "
    )
    applicantId: str = Field(
        ..., description="TalentSoft applicantId of the profile to fetch"
    )
    fileId: t.Optional[str] = Field(
        description=(
            "If provided only the attachment matching with fileId is left in"
            " 'attachments'. If not found all attachments are left"
        )
    )


def get_talentsoft_auth_token(
    client_url: str, token_scope: str, client_id: str, client_secret: str
) -> str:
    response = requests.post(
        "{}/api/token".format(client_url),
        headers={
            "Content-Type": "application/x-www-form-urlencoded",
        },
        data=dict(
            grant_type=GRANT_TYPE,
            scope=token_scope,
            client_id=client_id,
            client_secret=client_secret,
        ),
    )
    if not response.ok:
        raise Exception(
            "Failed to get authentication token with error={}".format(response.text)
        )
    try:
        return response.json()["access_token"]
    except (KeyError, requests.exceptions.JSONDecodeError) as e:
        raise Exception(
            "Failed to get token from response with error={}".format(repr(e))
        )


def read(
    adapter: LoggerAdapter, parameters: ReadProfileParameters
) -> t.Iterable[t.Dict]:
    token = get_talentsoft_auth_token(
        client_url=parameters.client_url,
        token_scope=parameters.token_scope,
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
    )
    response = requests.get(
        "{}/api/exports/v1/candidates?filter=id::{}".format(
            parameters.client_url, parameters.applicantId
        ),
        headers={
            "Authorization": "bearer {}".format(token),
        },
    )
    if not response.ok:
        raise Exception(
            "Failed to fetch candidate with applicantId={} from TalentSoft with"
            " error={}".format(parameters.applicantId, response.text)
        )
    if not response.content:
        adapter.warning(
            "Empty content when fetching candidate with applicantId={} from TalentSoft"
            .format(parameters.applicantId)
        )
        return

    data = response.content
    read_up_to = int.from_bytes(data[0:4], byteorder="little")
    zip_data = ZipFile(BytesIO(data[4 : 4 + read_up_to]))
    candidate = json.loads(zip_data.read("applicantdetail").decode())
    for attachment in candidate["attachments"]:
        attachment["raw"] = zip_data.read(attachment["id"])
    if parameters.fileId is not None:
        found = next(
            (
                attachment
                for attachment in candidate["attachments"]
                if attachment["id"] == parameters.fileId
            ),
            None,
        )
        if found:
            candidate["attachments"] = [found]
    yield candidate


TalentSoftProfileWarehouse = Warehouse(
    name="TalentSoft Profiles",
    read=WarehouseReadAction(
        parameters=ReadProfileParameters,
        function=read,
    ),
)
