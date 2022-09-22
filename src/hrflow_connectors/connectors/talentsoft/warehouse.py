import json
import typing as t
from io import BytesIO
from logging import LoggerAdapter
from zipfile import ZipFile

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.core import DataType, ReadMode, Warehouse, WarehouseReadAction

GRANT_TYPE = "client_credentials"
TOKEN_SCOPE = "MatchingIndexation"
LIMIT = 100


class ReadProfilesParameters(BaseModel):
    client_id: str = Field(
        ..., description="Client ID used to access TalentSoft API", repr=False
    )
    client_secret: str = Field(
        ..., description="Client Secret used to access TalentSoft API", repr=False
    )
    client_url: str = Field(..., description="URL of TalentSoft client integration")
    filter: t.Optional[str] = Field(
        None,
        description=(
            "Filter to apply when reading profiles. See documentation at"
            " https://developers.cegid.com/api-details#api=cegid-talentsoft-recruiting-"
            "matchingindexation&operation=api-exports-v1-candidates-get ."
            " Examples : By id Single Item 'id::_TS-00001'; By id Multiple"
            " Items 'id::_TS-00001,_TS-00002'; By email"
            " 'email::john.doe@company.corp'; By updateDate updated before the 10th"
            " of June 2019 'updateDate:lt:2019-06-10'; By chronoNumber greater than"
            " 108921  'chronoNumber:gt:108921'"
        ),
    )
    fileId: t.Optional[str] = Field(
        description=(
            "If provided only the attachment matching with fileId is left in"
            " 'attachments'. If not found all attachments are left."
        )
    )
    only_resume: bool = Field(
        False, description="If enabled only resume attachments are returned"
    )


class ReadJobsParameters(BaseModel):
    client_id: str = Field(
        ..., description="Client ID used to access TalentSoft API", repr=False
    )
    client_secret: str = Field(
        ..., description="Client Secret used to access TalentSoft API", repr=False
    )
    client_url: str = Field(..., description="URL of TalentSoft client integration")
    q: t.Optional[str] = Field(None, description="Query search to get vacancies")
    filter: t.Optional[str] = Field(
        None,
        description=(
            "Filter to apply when reading vacancies. See documentation at"
            " https://developers.cegid.com/api-details#api=cegid-talentsoft-recruiting"
            "-matchingindexation&operation=api-exports-v1-vacancies-get"
            " . . You can filter by **chronoNumber**, **updateDate**, **reference**"
            " **vacancyStatus**, **clientVacancyStatus**, **publicationMedia** "
            " **publishedOnTheMedia**. Examples : By reference Single Item"
            " 'reference::2019-01'; By reference Multiple Items"
            " 'reference::2019-01,2019-02,2019-03';  By updateDate updated before the"
            " 10th of June 2019 'updateDate:lt:2019-06-10'; By chronoNumber greater"
            " than 108921  'chronoNumber:gt:108921' . "
        ),
    )


def get_talentsoft_auth_token(
    client_url: str, client_id: str, client_secret: str
) -> str:
    response = requests.post(
        "{}/api/token".format(client_url),
        headers={
            "Content-Type": "application/x-www-form-urlencoded",
        },
        data=dict(
            grant_type=GRANT_TYPE,
            scope=TOKEN_SCOPE,
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


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token = get_talentsoft_auth_token(
        client_url=parameters.client_url,
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
    )
    params = dict(offset=0, limit=LIMIT)
    if parameters.filter:
        params["filter"] = parameters.filter
    if parameters.q:
        params["q"] = parameters.q

    while True:
        response = requests.get(
            "{}/api/exports/v1/vacancies".format(parameters.client_url),
            params=params,
            headers={
                "Authorization": "bearer {}".format(token),
            },
        )
        if not response.ok:
            raise Exception(
                "Failed to fetch jobs with params={} from TalentSoft with"
                " error={}".format(params, response.text)
            )
        if response.headers.get("Content-Length") == 0 or not response.content:
            if params["offset"] == 0:
                adapter.info(
                    "No jobs found with params={} text={} headers={}".format(
                        params, response.text, response.headers
                    )
                )
            return

        data = response.content
        while data:
            read_up_to = int.from_bytes(data[0:4], byteorder="little")
            zip_data = ZipFile(BytesIO(data[4 : 4 + read_up_to]))
            job = json.loads(zip_data.read("offerdetail").decode())["offerDetail"]
            yield job
            data = data[4 + read_up_to :]
        params["offset"] += LIMIT


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    token = get_talentsoft_auth_token(
        client_url=parameters.client_url,
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
    )
    params = dict(offset=0, limit=LIMIT)
    if parameters.filter:
        params["filter"] = parameters.filter
    if parameters.only_resume:
        params["attachmenttype"] = "resume"

    while True:
        response = requests.get(
            "{}/api/exports/v1/candidates".format(parameters.client_url),
            params=params,
            headers={
                "Authorization": "bearer {}".format(token),
            },
        )
        if not response.ok:
            raise Exception(
                "Failed to fetch candidates with params={} from TalentSoft with"
                " error={}".format(params, response.text)
            )
        if response.headers.get("Content-Length") == 0 or not response.content:
            if params["offset"] == 0:
                adapter.info(
                    "No profiles found with params={} text={} headers={}".format(
                        params, response.text, response.headers
                    )
                )
            return

        data = response.content
        while data:
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
            data = data[4 + read_up_to :]
        params["offset"] += LIMIT


TalentSoftProfilesWarehouse = Warehouse(
    name="TalentSoft Profiles",
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read_profiles,
    ),
)


TalentSoftJobsWarehouse = Warehouse(
    name="TalentSoft Jobs",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
    ),
)
