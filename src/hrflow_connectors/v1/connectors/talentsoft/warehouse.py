import json
import mimetypes
import typing as t
from datetime import date
from io import BytesIO
from logging import LoggerAdapter
from zipfile import ZipFile

import requests
import typing_extensions as te
from pydantic import Field, PositiveInt

from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    ReadMode,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)

GRANT_TYPE = "client_credentials"
TOKEN_SCOPE = "MatchingIndexation"
TOKEN_SCOPE_FULL_RIGHT = "Customer"
LIMIT = 100
TIMEOUT = 10
JOBS_DEFAULT_MAX_READ = 100


class ReadProfilesParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="Client ID used to access TalentSoft API",
        repr=False,
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        description="Client Secret used to access TalentSoft API",
        repr=False,
        field_type=FieldType.Auth,
    )
    client_url: str = Field(
        ...,
        description="URL of TalentSoft client integration",
        field_type=FieldType.Other,
    )
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
        field_type=FieldType.QueryParam,
    )
    fileId: t.Optional[str] = Field(
        description=(
            "If provided only the attachment matching with fileId is left in"
            " 'attachments'. If not found all attachments are left."
        ),
        field_type=FieldType.QueryParam,
    )
    only_resume: bool = Field(
        False,
        description="If enabled only resume attachments are returned",
        field_type=FieldType.QueryParam,
    )


class ReadJobsParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="Client ID used to access TalentSoft API",
        repr=False,
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        description="Client Secret used to access TalentSoft API",
        repr=False,
        field_type=FieldType.Auth,
    )
    client_url: str = Field(
        ...,
        description="URL of TalentSoft client integration",
        field_type=FieldType.Other,
    )
    q: t.Optional[str] = Field(
        None,
        description="Query search to get vacancies",
        field_type=FieldType.QueryParam,
    )
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
        field_type=FieldType.QueryParam,
    )
    max_read: t.Optional[PositiveInt] = Field(
        JOBS_DEFAULT_MAX_READ,
        description=(
            "The maximum number of jobs to pull during the execution. Proper tuning of"
            " this parameter should allow to control the execution time and avoid"
            " overtimes"
        ),
        field_type=FieldType.Other,
    )


class WriteProfileParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="client id used to access TalentSoft front office API",
        repr=False,
        field_type=FieldType.Auth,
    )
    client_secret: str = Field(
        ...,
        description="client secret used to access TalentSoft front office API",
        repr=False,
        field_type=FieldType.Auth,
    )
    client_url: str = Field(
        ...,
        description="url used to access TalentSoft front office API",
        repr=False,
        field_type=FieldType.Auth,
    )
    job_reference: str = Field(
        None,
        description="reference of the job offer to which the candidate is applying",
        repr=False,
        field_type=FieldType.Auth,
    )


def decode_unicode(input_str: str) -> str:
    try:
        return bytes(input_str, "utf-8").decode("unicode_escape")
    except UnicodeDecodeError:
        return input_str


def decode_json(
    obj: t.Union[str, list, dict, t.Any],
) -> t.Union[str, list, dict, t.Any]:
    if isinstance(obj, str):
        return decode_unicode(obj)
    elif isinstance(obj, list):
        return [decode_json(item) for item in obj]
    elif isinstance(obj, dict):
        return {key: decode_json(value) for key, value in obj.items()}
    else:
        return obj


def get_mime_type(filename: t.Optional[str]) -> str:
    if filename is None:
        return "application/octet-stream"
    mime_type, encoding = mimetypes.guess_type(filename)
    return mime_type or "application/octet-stream"


def get_cv_content(attachment: dict) -> t.Optional[bytes]:
    response = requests.get(attachment["public_url"])
    if response.status_code == 200:
        return response.content
    raise Exception(response.text)


def get_talentsoft_auth_token(
    client_url: str,
    client_id: str,
    client_secret: str,
    scope: str = TOKEN_SCOPE,
    front_or_back: te.Literal["back", "front"] = "back",
) -> str:
    if front_or_back == "front":
        data = dict(
            grant_type=GRANT_TYPE,
            client_id=client_id,
            client_secret=client_secret,
        )
    else:
        data = dict(
            grant_type=GRANT_TYPE,
            client_id=client_id,
            client_secret=client_secret,
            scope=scope,
        )
    response = requests.post(
        "{}/api/token".format(client_url),
        headers={
            "Content-Type": "application/x-www-form-urlencoded",
        },
        timeout=TIMEOUT,
        data=data,
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


def post_applicant_front(client_url, token, applicant, files, job_reference=None):
    headers = {
        "Authorization": "Bearer " + token,
    }
    if job_reference:
        headers.update(
            {
                "Accept-Language": "fr-FR",
                "jobAdReference": job_reference,
            }
        )
    response = requests.post(
        "{}/api/v2/applicants/applicationswithoutaccount".format(client_url),
        headers=headers,
        data=applicant,
        files=files,
    )
    if response.status_code == 201:
        return response.json()
    raise Exception(response.text)


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadProfilesParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    if parameters.filter and read_mode is ReadMode.incremental:
        raise Exception("filter cannot be set when read_mode is incremental")

    adapter.info("Requesting Authentication Token from TS")
    token = get_talentsoft_auth_token(
        client_url=parameters.client_url,
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
    )
    adapter.info("Authentication with TS API Endpoint finished")

    limit = min(LIMIT, parameters.max_read)
    params = dict(offset=0, limit=limit)
    if read_mode is ReadMode.incremental:
        if read_from is None:
            read_from = date.today().isoformat()
        params["filter"] = "updateDate:gt:{}".format(read_from)
    elif parameters.filter:
        params["filter"] = parameters.filter

    if parameters.q:
        params["q"] = parameters.q

    item_counter = 0
    while True:
        response = requests.get(
            "{}/api/exports/v1/vacancies".format(parameters.client_url),
            params=params,
            timeout=TIMEOUT,
            headers={
                "Authorization": "bearer {}".format(token),
            },
        )
        if not response.ok:
            raise Exception(
                "Failed to fetch jobs with params={} from TalentSoft with error={}"
                .format(params, response.text)
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
            item_counter += 1
            if item_counter >= parameters.max_read:
                return
            data = data[4 + read_up_to :]
        params["offset"] += limit


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
            timeout=TIMEOUT,
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


def write_profiles(
    adapter: LoggerAdapter,
    parameters: WriteProfileParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    adapter.info("Requesting Authentication Token from TS")
    token = get_talentsoft_auth_token(
        client_url=parameters.client_url,
        client_id=parameters.client_id,
        client_secret=parameters.client_secret,
        front_or_back="front",
    )
    adapter.info("Authentication with TS API Endpoint finished")
    for profile in profiles:
        attachment = profile.pop("attachment", None)
        if not attachment:
            adapter.error("No attachment found for profile={}".format(profile))
            failed_profiles.append(profile)
            continue
        try:
            cv_content = get_cv_content(attachment)
        except Exception as e:
            adapter.error("Failed to get cv content with response={}".format(e))
            failed_profiles.append(profile)
            continue
        filename = attachment["original_file_name"]
        mime_type = get_mime_type(filename)
        files = [
            (
                "cv_file_id",
                (filename, cv_content, mime_type),
            )
        ]
        if parameters.job_reference:
            profile["application"]["offerReference"] = parameters.job_reference
        profile = decode_json(profile)
        profile_ts = dict(applicantApplication=json.dumps(profile))
        try:
            post_applicant_front(
                parameters.client_url,
                token,
                profile_ts,
                files,
                parameters.job_reference,
            )
        except Exception as e:
            adapter.error("Failed to write profile with response={}".format(e))
            failed_profiles.append(profile)

    return failed_profiles


TalentSoftProfilesWarehouse = Warehouse(
    name="TalentSoft Profiles",
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfilesParameters,
        function=read_profiles,
    ),
    write=WarehouseWriteAction(
        parameters=WriteProfileParameters,
        function=write_profiles,
    ),
)


TalentSoftJobsWarehouse = Warehouse(
    name="TalentSoft Jobs",
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        supports_incremental=True,
        item_to_read_from=lambda job: job["modificationDate"],
    ),
)
