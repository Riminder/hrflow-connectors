import typing as t
from logging import LoggerAdapter

import requests
from pydantic import Field

from hrflow_connectors.connectors.bullhorn.schemas import BullhornJob, BullhornProfile
from hrflow_connectors.connectors.bullhorn.utils.authentication import auth
from hrflow_connectors.core import (
    DataType,
    FieldType,
    ParametersModel,
    Warehouse,
    WarehouseReadAction,
    WarehouseWriteAction,
)
from hrflow_connectors.core.warehouse import ReadMode


class WriteProfilesParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="Client identifier for Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    client_secret: str = Field(
        ...,
        description="Client secret identifier for Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    password: str = Field(
        ...,
        description="Passoword for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )

    username: str = Field(
        ...,
        description="Username for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadJobsParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="Client identifier for Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    client_secret: str = Field(
        ...,
        description="Client secret identifier for Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    password: str = Field(
        ...,
        description="Passoword for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )

    username: str = Field(
        ...,
        description="Username for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )


class ReadProfileParameters(ParametersModel):
    client_id: str = Field(
        ...,
        description="Client identifier for Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    client_secret: str = Field(
        ...,
        description="Client secret identifier for Bullhorn",
        repr=False,
        field_type=FieldType.Auth,
    )

    password: str = Field(
        ...,
        description="Passoword for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )

    username: str = Field(
        ...,
        description="Username for Bullhorn login",
        repr=False,
        field_type=FieldType.Auth,
    )


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    profiles: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    adapter.info("Pushing {} profiles".format(len(profiles)))
    failed_profiles = []
    authentication = auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )

    for profile in profiles:
        # Split profile in four parts: Body, Education, Experience and Attachements
        profile_body_dict = profile
        create_profile_body = profile_body_dict["create_profile_body"]
        enrich_profile_education = profile_body_dict["enrich_profile_education"]
        enrich_profile_experience = profile_body_dict["enrich_profile_experience"]
        enrich_profile_attachment = profile_body_dict["enrich_profile_attachment"]

        rest_url = authentication["restUrl"]
        params = {"BhRestToken": authentication["BhRestToken"]}

        candidate_url = rest_url + "entity/Candidate"
        response = requests.put(
            url=candidate_url, json=create_profile_body, params=params
        )

        # Unable to push profile
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to push profile from to Bullhorn"
                " status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            failed_profiles.append(profile)
            continue

        candidate_id = response.json()
        candidate_id = str(candidate_id["changedEntityId"])

        # Enrich profile education
        for education in enrich_profile_education:
            education["candidate"]["id"] = candidate_id
            education_url = rest_url + "entity/CandidateEducation"
            response = requests.put(url=education_url, json=education, params=params)

        # Enrich profile experience
        for experience in enrich_profile_experience:
            experience["candidate"]["id"] = candidate_id
            experience_url = rest_url + "entity/CandidateWorkHistory"
            response = requests.put(url=experience_url, json=experience, params=params)

        # Enrich profile attachements
        for attachment in enrich_profile_attachment:
            attachment_url = rest_url + "file/Candidate/" + str(candidate_id)
            response = requests.put(url=attachment_url, json=attachment, params=params)

    return failed_profiles


def read_jobs(
    adapter: LoggerAdapter,
    parameters: ReadJobsParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    authentication = auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )
    start = 0

    while True:
        jobs_url = (
            authentication["restUrl"]
            + "search/JobOrder?query=(isOpen:true)&fields=*&BhRestToken="
            + authentication["BhRestToken"]
            + "&start="
            + str(start)
        )

        response = requests.get(url=jobs_url)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull jobs from Bullhorn status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            raise Exception("Failed to pull jobs from Bullhorn")
        response = response.json()

        start = response["start"] + response["count"]
        data = response["data"]

        for job in data:
            yield job

        if start >= response["total"]:
            break


def read_profiles_parsing(
    adapter: LoggerAdapter,
    parameters: ReadProfileParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    authentication = auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )
    start = 0
    while True:
        profiles_url = (
            authentication["restUrl"] + "myCandidates?fields=*&start=" + str(start)
        )
        headers = {"BhRestToken": authentication["BhRestToken"]}

        response = requests.get(url=profiles_url, headers=headers)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profiles from Bullhorn"
                " status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            raise Exception("Failed to pull profiles from Bullhorn")
        response = response.json()

        start = response["start"] + response["count"]
        data = response["data"]
        total = response["total"]

        for profile in data:
            profile["cvFile"] = None
            url_files = (
                authentication["restUrl"]
                + "entityFiles/Candidate/"
                + str(profile["id"])
            )
            headers = {"BhRestToken": authentication["BhRestToken"]}
            response = requests.get(url=url_files, headers=headers)
            response = response.json()

            last_cv = None
            curr_entity = None
            if len(response["EntityFiles"]) > 0:
                for entity_file in response["EntityFiles"]:
                    if entity_file["type"] == "Resume":
                        if not curr_entity:
                            curr_entity = entity_file
                            last_cv = entity_file["id"]
                        elif curr_entity["dateAdded"] < entity_file["dateAdded"]:
                            curr_entity = entity_file
                            last_cv = entity_file["id"]

            if last_cv is not None:
                url_cv = (
                    authentication["restUrl"]
                    + "/file/Candidate/"
                    + str(profile["id"])
                    + "/"
                    + str(last_cv)
                    + "/raw"
                )
                response = requests.get(url=url_cv, headers=headers)

                file = response.content
                with open("cv_file.pdf", "wb") as f:
                    f.write(file)

                with open("cv_file.pdf", "rb") as f:
                    profile_file = f.read()
                    profile["cvFile"] = profile_file

                yield profile

        if start >= total:
            break


def read_profiles(
    adapter: LoggerAdapter,
    parameters: ReadProfileParameters,
    read_mode: t.Optional[ReadMode] = None,
    read_from: t.Optional[str] = None,
) -> t.Iterable[t.Dict]:
    authentication = auth(
        parameters.username,
        parameters.password,
        parameters.client_id,
        parameters.client_secret,
    )
    fields = """firstName,lastName,name,email,mobile,dateOfBirth,gender,address,dateAvailable,status,employeeType,activePlacements,
    skillSet,id,educations,workHistories"""
    start = 0
    while True:
        profiles_url = (
            authentication["restUrl"]
            + "myCandidates?fields="
            + fields
            + "&start="
            + str(start)
        )
        headers = {"BhRestToken": authentication["BhRestToken"]}

        response = requests.get(url=profiles_url, headers=headers)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to pull profiles from Bullhorn"
                " status_code={} response={}".format(
                    response.status_code, response.text
                )
            )
            raise Exception("Failed to pull profiles from Bullhorn")
        response = response.json()

        start = response["start"] + response["count"]
        total = response["total"]
        data = response["data"]

        for profile in data:
            # Enrich profile education
            education_ids = []
            educations = []
            for ed in profile["educations"]["data"]:
                education_ids.append(ed["id"])
            for id in education_ids:
                education_url = (
                    authentication["restUrl"]
                    + "entity/CandidateEducation/"
                    + str(id)
                    + "?fields=*"
                )
                response = requests.get(url=education_url, headers=headers)
                response = response.json()
                educations.append(response["data"])

            # Enrich profile work history
            work_history_ids = []
            work_histories = []
            for work_history in profile["workHistories"]["data"]:
                work_history_ids.append(work_history["id"])
            for id in work_history_ids:
                work_history_url = (
                    authentication["restUrl"]
                    + "entity/CandidateWorkHistory/"
                    + str(id)
                    + "?fields=*"
                )
                response = requests.get(url=work_history_url, headers=headers)
                response = response.json()
                work_histories.append(response["data"])

            profile["educations"] = educations
            profile["workHistories"] = work_histories

            yield profile

        if start >= total:
            break


BullhornProfileWarehouse = Warehouse(
    name="Bullhorn Profiles",
    data_schema=BullhornProfile,
    data_type=DataType.profile,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters, function=write, endpoints=[]
    ),
    read=WarehouseReadAction(
        parameters=ReadProfileParameters, function=read_profiles, endpoints=[]
    ),
)

BullhornProfileParsingWarehouse = Warehouse(
    name="Bullhorn Profiles",
    data_schema=BullhornProfile,
    data_type=DataType.profile,
    read=WarehouseReadAction(
        parameters=ReadProfileParameters, function=read_profiles_parsing, endpoints=[]
    ),
)

BullhornJobWarehouse = Warehouse(
    name="Bullhorn Jobs",
    data_schema=BullhornJob,
    data_type=DataType.job,
    read=WarehouseReadAction(
        parameters=ReadJobsParameters,
        function=read_jobs,
        endpoints=[],
    ),
)
