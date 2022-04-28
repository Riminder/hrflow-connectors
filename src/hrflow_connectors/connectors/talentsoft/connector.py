import typing as t
from datetime import datetime

import requests
from pydantic import BaseModel

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowProfileParsingWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.talentsoft.warehouse import (
    TalentSoftProfilesWarehouse,
    get_talentsoft_auth_token,
)
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    Event,
    WorkflowType,
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
                "Host": "safran-rh.profils.org",  # FIXME REMOVE ME
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


DESCRIPTION = "TalentSoft"
TalentSoft = Connector(
    name="TalentSoft",
    description=DESCRIPTION,
    url="https://www.cegid.com/fr/produits/cegid-talentsoft/",
    actions=[
        ConnectorAction(
            name="applicant_new",
            type=WorkflowType.catch,
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
        ),
        ConnectorAction(
            name="applicant_resume_update",
            type=WorkflowType.catch,
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
        ),
        ConnectorAction(
            name="applicant_update",
            type=WorkflowType.catch,
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
        ),
        ConnectorAction(
            name="pull_profiles",
            type=WorkflowType.pull,
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
        ),
    ],
)
