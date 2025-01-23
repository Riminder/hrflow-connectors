import json
import mimetypes
import typing as t
from datetime import date
from io import BytesIO
from logging import LoggerAdapter
from zipfile import ZipFile

import requests
from msgspec import Meta, Struct
from typing_extensions import Annotated, Literal

from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    IncrementalTokenHandler,
    ReadOperation,
    WriteOperation,
    merge,
)

GRANT_TYPE = "client_credentials"
TOKEN_SCOPE = "MatchingIndexation"
TOKEN_SCOPE_FULL_RIGHT = "Customer"
LIMIT = 100
TIMEOUT = 10
JOBS_DEFAULT_MAX_READ = 100


class AuthParameters(Struct):
    client_id: Annotated[
        str,
        Meta(
            description="client id used to access TalentSoft front office API",
        ),
    ]
    client_secret: Annotated[
        str,
        Meta(
            description="client secret used to access TalentSoft front office API",
        ),
    ]
    client_url: Annotated[
        str,
        Meta(
            description="url used to access TalentSoft front office API",
        ),
    ]


class ReadProfilesParameters(Struct):
    filter: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Filter to apply when reading profiles. See documentation at"
                " https://developers.cegid.com/api-details#api=cegid-talentsoft"
                "-recruiting-matchingindexation&operation=api-exports-v1-candidates-get"
                " . Examples : By id Single Item 'id::_TS-00001'; By id Multiple Items"
                " 'id::_TS-00001,_TS-00002'; By email 'email::john.doe@company.corp';"
                " By updateDate updated before the 10th of June 2019"
                " 'updateDate:lt:2019-06-10'; By chronoNumber greater than 108921 "
                " 'chronoNumber:gt:108921'"
            ),
        ),
    ] = None


class ReadJobsParameters(Struct):
    q: Annotated[
        t.Optional[str],
        Meta(
            description="Query search to get vacancies",
        ),
    ] = None
    filter: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Filter to apply when reading vacancies. See documentation at"
                " https://developers.cegid.com/api-details#api=cegid-talentsoft"
                "-recruiting-matchingindexation&operation=api-exports-v1-vacancies-get"
                " . . You can filter by **chronoNumber**, **updateDate**, **reference**"
                " **vacancyStatus**, **clientVacancyStatus**, **publicationMedia** "
                " **publishedOnTheMedia**. Examples : By reference Single Item"
                " 'reference::2019-01'; By reference Multiple Items"
                " 'reference::2019-01,2019-02,2019-03';  By updateDate updated before"
                " the 10th of June 2019 'updateDate:lt:2019-06-10'; By chronoNumber"
                " greater than 108921  'chronoNumber:gt:108921' . "
            ),
        ),
    ] = None
    max_read: Annotated[
        int,
        Meta(
            description=(
                "The maximum number of jobs to pull during the execution. Proper tuning"
                " of this parameter should allow to control the execution time and"
                " avoid overtimes"
            ),
        ),
    ] = JOBS_DEFAULT_MAX_READ


class WriteProfileParameters(Struct):
    job_reference: Annotated[
        str,
        Meta(
            description="reference of the job offer to which the candidate is applying",
        ),
    ]


class UpdateProfileParameters(Struct):
    pass


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
    front_or_back: Literal["back", "front"] = "back",
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


def update_applicant_front(client_url, token, applicant):
    headers = {
        "Authorization": "Bearer " + token,
    }
    response = requests.put(
        "{}/api/v2/applicants".format(client_url),
        headers=headers,
        data=applicant,
    )
    if response.status_code == 204:
        return response.json()
    raise Exception(response.text)


def read_jobs(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadJobsParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    if parameters.filter and incremental:
        raise Exception("filter cannot be set when incremental is enabled")

    adapter.info("Requesting Authentication Token from TS")
    token = get_talentsoft_auth_token(
        client_url=auth_parameters.client_url,
        client_id=auth_parameters.client_id,
        client_secret=auth_parameters.client_secret,
    )
    adapter.info("Authentication with TS API Endpoint finished")

    limit = min(LIMIT, parameters.max_read)
    params: dict[str, t.Union[str, int]] = dict(offset=0, limit=limit)
    if incremental:
        if incremental_token is None:
            incremental_token = date.today().isoformat()
        params["filter"] = "updateDate:gt:{}".format(incremental_token)
    elif parameters.filter:
        params["filter"] = parameters.filter

    if parameters.q:
        params["q"] = parameters.q

    item_counter = 0
    while True:
        response = requests.get(
            "{}/api/exports/v1/vacancies".format(auth_parameters.client_url),
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
        params["offset"] = t.cast(int, params["offset"]) + limit


def read_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: ReadProfilesParameters,
    incremental: bool,
    incremental_token: t.Optional[str],
) -> t.Iterable[t.Dict]:
    token = get_talentsoft_auth_token(
        client_url=auth_parameters.client_url,
        client_id=auth_parameters.client_id,
        client_secret=auth_parameters.client_secret,
    )
    params = dict(offset=0, limit=LIMIT, attachmenttype="resume")
    if parameters.filter:
        params["filter"] = parameters.filter

    while True:
        response = requests.get(
            "{}/api/exports/v1/candidates".format(auth_parameters.client_url),
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
            yield candidate
            data = data[4 + read_up_to :]
        params["offset"] = t.cast(int, params["offset"]) + LIMIT


def write_profiles(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfileParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    adapter.info("Requesting Authentication Token from TS")
    token = get_talentsoft_auth_token(
        client_url=auth_parameters.client_url,
        client_id=auth_parameters.client_id,
        client_secret=auth_parameters.client_secret,
        front_or_back="front",
    )
    adapter.info("Authentication with TS API Endpoint finished")
    for profile in items:
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
                auth_parameters.client_url,
                token,
                profile_ts,
                files,
                parameters.job_reference,
            )
        except Exception as e:
            adapter.error("Failed to write profile with response={}".format(e))
            failed_profiles.append(profile)

    return failed_profiles


def update_profile(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: UpdateProfileParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    adapter.info("Requesting Authentication Token from TS")
    token = get_talentsoft_auth_token(
        client_url=auth_parameters.client_url,
        client_id=auth_parameters.client_id,
        client_secret=auth_parameters.client_secret,
        front_or_back="front",
    )
    adapter.info("Authentication with TS API Endpoint finished")
    for profile in items:
        try:
            update_applicant_front(
                auth_parameters.client_url,
                token,
                profile,
            )
        except Exception as e:
            adapter.error("Failed to write profile with response={}".format(e))
            failed_profiles.append(profile)

    return failed_profiles


ProfilesAisle = Aisle(
    name=Entity.profile,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadProfilesParameters,
            update=ReadProfilesParameters,
            archive=ReadProfilesParameters,
        ),
        function=merge(
            create=read_profiles,
            update=read_profiles,
            archive=read_profiles,
        ),
    ),
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfileParameters, update=UpdateProfileParameters
        ),
        function=merge(
            create=write_profiles,
            update=update_profile,
        ),
    ),
)


JobsAisle = Aisle(
    name=Entity.job,
    read=ReadOperation(
        criterias=Criterias(
            create=ReadJobsParameters,
            update=ReadJobsParameters,
            archive=ReadJobsParameters,
        ),
        function=merge(
            create=read_jobs,
            update=read_jobs,
            archive=read_jobs,
        ),
        incremental_token_handler=IncrementalTokenHandler(
            create=lambda job: job["modificationDate"],
        ),
    ),
)
