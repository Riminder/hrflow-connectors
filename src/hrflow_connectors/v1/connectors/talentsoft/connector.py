import typing as t
from datetime import datetime

import requests
from pydantic import BaseModel

from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    Event,
    WorkflowType,
)
from hrflow_connectors.v1.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileParsingWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.v1.connectors.talentsoft.utils.const import (
    CIVILITY,
    CONTRACT_TYPE_REFERENTIAL,
    EDUCATIONS_REFERENTIEL,
    EXPERIENCES_REFERENTIEL,
)
from hrflow_connectors.v1.connectors.talentsoft.warehouse import (
    TalentSoftJobsWarehouse,
    TalentSoftProfilesWarehouse,
    get_talentsoft_auth_token,
)


def retrieve_tag_value(tags: t.List[dict], tag_name: str) -> t.Any:
    for tag in tags:
        if tag["name"] == tag_name:
            return tag["value"]
    return None


def format_ts_applicant_civility(gender: t.Optional[str]) -> t.Optional[t.Dict]:
    civility_ts = {}
    if gender is None:
        return None
    if gender == "male":
        return CIVILITY[2]
    elif gender == "female":
        return CIVILITY[4]
    return civility_ts


def extract_year(date_str: str) -> t.Optional[int]:
    if not date_str:
        return None
    try:
        return datetime.fromisoformat(date_str).year
    except ValueError:
        return None


def calcul_ts_experience_duration(date_start: str, date_end: str) -> t.Optional[float]:
    if not date_start or not date_end:
        return None
    try:
        date_start_obj = datetime.fromisoformat(date_start)
        date_end_obj = datetime.fromisoformat(date_end)
        return (date_end_obj - date_start_obj).days / 365
    except ValueError:
        return None


def format_contract_type(tags: t.List[dict]) -> t.Optional[t.Dict]:
    contract_type = retrieve_tag_value(tags, "talentsoft_contract_type")
    if contract_type:
        return CONTRACT_TYPE_REFERENTIAL.get(contract_type)
    return None


def format_ts_educations(educations: t.List[dict], tags: t.List[dict]) -> dict:
    education_level = retrieve_tag_value(tags, "talentsoft_education_level")

    diplomas = (
        [{"educationLevel": EDUCATIONS_REFERENTIEL.get(education_level)}]
        if education_level
        else []
    )

    diplomas += [
        {
            "yearObtained": (
                extract_year(education["date_end"]) if education["date_end"] else ""
            ),
            "college": education["school"],
        }
        for education in educations
    ]

    return {"diplomas": diplomas}


def format_ts_experiences(experiences: t.List[dict], tags: t.List[dict]) -> dict:
    experience_level = retrieve_tag_value(tags, "talentsoft_experience_level")
    experience_ts_level = experience_level if experience_level else None
    experiences_ts_list = [
        {
            "company": experience["company"],
            "function": experience["title"],
            "length": (
                calcul_ts_experience_duration(
                    experience["date_start"], experience["date_end"]
                )
                if experience["date_start"] and experience["date_end"]
                else ""
            ),
        }
        for experience in experiences
    ]

    experiences_ts = {
        "experienceLevel": EXPERIENCES_REFERENTIEL.get(experience_ts_level),
        "experienceList": experiences_ts_list,
    }

    return experiences_ts


def format_ts_vacancy(ts_vacancy: t.Dict) -> t.Dict:
    # FIXME lat and lng makes requests to HERE Maps API in original workflow
    ts_location = ts_vacancy["location"]
    location = dict(
        text=ts_location["address"],
        lat=ts_location["latitude"],
        lng=ts_location["longitude"],
    )

    job_description = ts_vacancy["jobDescription"]
    custom_fields = job_description["jobDescriptionCustomFields"]
    sections = [
        dict(
            name="description1",
            title="description1",
            description=job_description["description1"] or "",
        ),
        dict(
            name="description2",
            title="description2",
            description=job_description["description2"] or "",
        ),
        dict(
            name="Complément du descriptif",
            title="Complément du descriptif",
            description="\n".join(
                [
                    custom_fields.get("longText1") or "",
                    custom_fields.get("longText2") or "",
                    custom_fields.get("longText3") or "",
                ]
            ),
        ),
    ]

    languages = [
        dict(name=language["language"]["label"], value=None)
        for language in ts_vacancy.get("languages") or []
    ]

    tags = [
        dict(
            name="talentsoft-organisation-id",
            value=(ts_vacancy["organisation"] or {}).get("id"),
        ),
        dict(
            name="talentsoft-status-id",
            value=(ts_vacancy["status"] or {}).get("id"),
        ),
        dict(
            name="talentsoft-professionalCategory-id",
            value=(ts_vacancy["jobDescription"]["professionalCategory"] or {}).get(
                "id"
            ),
        ),
        dict(
            name="talentsoft-country-id",
            value=(ts_vacancy["jobDescription"]["country"] or {}).get("id"),
        ),
        dict(
            name="talentsoft-primaryProfile-id",
            value=(ts_vacancy["jobDescription"]["primaryProfile"] or {}).get("id"),
        ),
        dict(
            name="talentsoft-contractType-id",
            value=(ts_vacancy["jobDescription"]["contractType"] or {}).get("id"),
        ),
        dict(
            name="talentsoft-publishedOnInternet",
            value=ts_vacancy.get("publishedOnInternet"),
        ),
        dict(
            name="talentsoft-publishedOnIntranet",
            value=ts_vacancy.get("publishedOnIntranet"),
        ),
    ]

    if ts_vacancy["criteria"]["experienceLevel"]:
        tags.append(
            dict(
                name="talentsoft-experienceLevel",
                value=ts_vacancy["criteria"]["experienceLevel"].get("id"),
            )
        )

    if ts_vacancy["criteria"]["educationLevel"]:
        tags.append(
            dict(
                name="talentsoft-educationLevel",
                value=ts_vacancy["criteria"]["educationLevel"].get("id"),
            )
        )

    return dict(
        name=ts_vacancy["jobDescription"]["title"],
        reference=ts_vacancy["reference"],
        created_at=ts_vacancy["creationDate"],
        location=location,
        url=None,
        summary=None,
        sections=sections,
        tags=tags,
        skills=ts_vacancy["criteria"]["skills"],
        languages=languages,
    )


def format_ts_candidate(ts_candidate: t.Dict) -> t.Dict:
    details = ts_candidate["candidateDetail"]
    tags = [
        dict(name="talentsoft-isEmployee", value=ts_candidate["isEmployee"]),
        dict(name="talentsoft-isInProgress", value=ts_candidate["isInProgress"]),
        dict(
            name="talentsoft-residentCountry-id",
            value=details["personalInformation"]["residentCountry"].get("id"),
        ),
    ]

    contract_type = details["positionSought"]["contractType"]
    if contract_type:
        tags.append(
            dict(
                name="talentsoft-contractType-id",
                value=contract_type["id"],
            )
        )

    custom_code_table_1 = details["positionSought"]["jobPreferencesCustomFields"][
        "customCodeTable1"
    ]
    if custom_code_table_1:
        tags.append(
            dict(
                name="talentsoft-profileStatus-id",
                value=custom_code_table_1["id"],
            )
        )

    global_experience_level = details["globalExperience"]["globalExperienceLevel"]
    if global_experience_level:
        tags.append(
            dict(
                name="talentsoft-experienceLevel-id",
                value=global_experience_level["id"],
            )
        )

    primary_profile = details["positionSought"]["primaryProfile"]
    if primary_profile:
        tags.append(
            dict(
                name="talentsoft-profile-id",
                value=primary_profile["id"],
            )
        )

    for education in details["educations"]:
        if education["educationLevel"]:
            tags.append(
                {
                    "name": "talentsofteducationLevel-id",
                    "value": education["educationLevel"]["id"],
                }
            )
    for application in ts_candidate["applications"]:
        tags.append(
            dict(
                name="talentsoft-application-vacancyReference",
                value=application["vacancyReference"],
            )
        )

    metadatas = [
        dict(name="profile_uid", value=details["id"]),
    ]

    resume = None
    attachment = next(
        (
            attachment
            for attachment in ts_candidate["attachments"]
            if attachment["isResume"]
        ),
        None,
    )
    if attachment:
        resume = dict(raw=attachment["raw"], content_type=attachment["mimeType"])
        metadatas.append(dict(name="filename", value=attachment["filename"]))
    return dict(
        reference=details["id"],
        created_at=details["creationDate"],
        updated_at=datetime.utcnow().isoformat(),
        resume=resume,
        tags=tags,
        metadatas=metadatas,
    )


def ts_callback(
    origin_parameters: BaseModel,
    target_parameters: BaseModel,
    events: t.Counter[Event],
    items_to_write: t.List[t.Dict],
) -> None:
    if len(items_to_write) == 1 and events[Event.write_failure] == 0:
        candidate = items_to_write[0]
        token = get_talentsoft_auth_token(
            client_url=origin_parameters.client_url,
            client_id=origin_parameters.client_id,
            client_secret=origin_parameters.client_secret,
        )
        report = dict(
            reportType="Candidates",
            items=[
                dict(
                    id=candidate["reference"],
                    date=candidate["created_at"],
                    lastUpdateDate=candidate["updated_at"],
                    status="success",
                )
            ],
        )
        response = requests.post(
            "{}/api/exports/v1/reports".format(origin_parameters.client_url),
            headers={
                "Authorization": "bearer {}".format(token),
            },
            json=report,
        )
        if not response.ok:
            raise Exception(
                "Talentsoft callback report failed error={} evens={}".format(
                    response.text,
                    events,
                )
            )


def applicant_new_parser(event: t.Dict) -> t.Dict:
    return dict(filter="id::{}".format(event["applicantId"]))


def applicant_resume_update_parser(event: t.Dict) -> t.Dict:
    return dict(filter="id::{}".format(event["applicantId"]), fileId=event["fileId"])


def applicant_update_parser(event: t.Dict) -> t.Dict:
    return dict(filter="id::{}".format(event["applicantId"]))


def format_info_ts_applicant(profile_hrflow: t.Dict) -> t.Dict:
    info_profile_hrflow = profile_hrflow["info"]
    attachment = profile_hrflow["attachments"][0]
    personal_information = dict(
        firstName=info_profile_hrflow["first_name"],
        lastName=info_profile_hrflow["last_name"],
        birthDate=info_profile_hrflow["date_birth"],
        phoneNumber=info_profile_hrflow["phone"],
        email=info_profile_hrflow["email"],
        address=info_profile_hrflow["location"]["text"],
        city=(
            info_profile_hrflow["location"].get("fields", {}).get("city", "")
            if info_profile_hrflow["location"].get("fields")
            else ""
        ),
        postalCode=(
            info_profile_hrflow["location"].get("fields", {}).get("postalCode", "")
            if info_profile_hrflow["location"].get("fields")
            else ""
        ),
        civility=format_ts_applicant_civility(info_profile_hrflow["gender"]),
    )
    jobPreferences = dict(
        contractType=format_contract_type(profile_hrflow["tags"]),
        salaryPretensions=retrieve_tag_value(
            profile_hrflow["tags"], "talentsoft_salary"
        ),
    )
    education = format_ts_educations(
        profile_hrflow["educations"], profile_hrflow["tags"]
    )
    experiences = format_ts_experiences(
        profile_hrflow["experiences"], profile_hrflow["tags"]
    )
    application = dict(
        type="offer",
        offerReference="",  # TODO
    )
    uploadedFiles = [
        dict(
            description=attachment["original_file_name"],
            fileTypeId=36,
            key="cv_file_id",
        )
    ]

    applicant = dict(
        personalInformation=personal_information,
        jobPreferences=jobPreferences,
        education=education,
        experiences=experiences,
    )

    return dict(
        applicant=applicant,
        application=application,
        uploadedFiles=uploadedFiles,
        attachment=attachment,
    )


DESCRIPTION = "TalentSoft"
TalentSoft = Connector(
    name="TalentSoft",
    type=ConnectorType.HCM,
    subtype="talentsoft",
    description=DESCRIPTION,
    url="https://www.cegid.com/fr/produits/cegid-talentsoft/",
    actions=[
        ConnectorAction(
            name=ActionName.applicant_new,
            trigger_type=WorkflowType.catch,
            description=(
                "Handle TalentSoft 'applicant_new' event by fetching profile from"
                " TalentSoft and sending it to HrFlow.ai Parsing API."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ApplicantNewActionParameters",
                format=format_ts_candidate,
                event_parser=applicant_new_parser,
            ),
            origin=TalentSoftProfilesWarehouse.with_fixed_read_parameters(
                only_resume=True
            ),
            target=HrFlowProfileParsingWarehouse.with_fixed_write_parameters(
                only_insert=True
            ),
            callback=ts_callback,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.applicant_resume_update,
            trigger_type=WorkflowType.catch,
            description=(
                "Handle TalentSoft 'applicant_resume_update' event by"
                " running a new HrFlow.ai Parsing on updated resume."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ApplicantResumeUpdateActionParameters",
                format=format_ts_candidate,
                event_parser=applicant_resume_update_parser,
            ),
            origin=TalentSoftProfilesWarehouse.with_fixed_read_parameters(
                only_resume=True
            ),
            target=HrFlowProfileParsingWarehouse.with_fixed_write_parameters(
                only_insert=False
            ),
            callback=ts_callback,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.applicant_update,
            trigger_type=WorkflowType.catch,
            description=(
                "Handle TalentSoft 'applicant_update' event by"
                " only updating tags coming from TalentSoft in HrFlow.ai."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ApplicantUpdateActionParameters",
                format=format_ts_candidate,
                event_parser=applicant_update_parser,
            ),
            origin=TalentSoftProfilesWarehouse,
            target=HrFlowProfileWarehouse.with_fixed_write_parameters(
                edit=True, only_edit_fields=["tags"]
            ),
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves profiles from TalentSoft candidates export API and send them"
                " to a ***Hrflow.ai Source***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "PullTalentSoftProfilesActionParameters",
                format=format_ts_candidate,
            ),
            origin=TalentSoftProfilesWarehouse.with_fixed_read_parameters(
                only_resume=True
            ),
            target=HrFlowProfileParsingWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs from TalentSoft vacancies export API and send them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "PullTalentSoftProfilesActionParameters",
                format=format_ts_vacancy,
            ),
            origin=TalentSoftJobsWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Pushs specific Profile from HrFlow and writes"
                " it to Applicant object in Talentsoft"
            ),
            parameters=BaseActionParameters.with_defaults(
                "PushProfileActionParameters", format=format_info_ts_applicant
            ),
            origin=HrFlowProfileWarehouse,
            target=TalentSoftProfilesWarehouse,
            action_type=ActionType.outbound,
        ),
    ],
)
