import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowProfileParsingWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.talentsoft.warehouse import TalentSoftProfileWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
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
        resume=resume,
        tags=tags,
        metadatas=metadatas,
    )


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
            parameters=BaseActionParameters.with_default_format(
                "ApplicantNewActionParameters", format=format_ts_candidate
            ),
            origin=TalentSoftProfileWarehouse.with_fixed_read_parameters(
                token_scope="MatchingIndexation"
            ),
            target=HrFlowProfileParsingWarehouse.with_fixed_write_parameters(
                only_insert=True
            ),
        ),
        ConnectorAction(
            name="applicant_resume_update",
            type=WorkflowType.catch,
            description=(
                "Handle TalentSoft 'applicant_resume_update' event by"
                " running a new HrFlow.ai Parsing on updated resume."
            ),
            parameters=BaseActionParameters.with_default_format(
                "ApplicantResumeUpdateActionParameters", format=format_ts_candidate
            ),
            origin=TalentSoftProfileWarehouse.with_fixed_read_parameters(
                token_scope="MatchingIndexation"
            ),
            target=HrFlowProfileParsingWarehouse.with_fixed_write_parameters(
                only_insert=False
            ),
        ),
        ConnectorAction(
            name="applicant_update",
            type=WorkflowType.catch,
            description=(
                "Handle TalentSoft 'applicant_update' event by"
                " only updating tags coming from TalentSoft in HrFlow.ai."
            ),
            parameters=BaseActionParameters.with_default_format(
                "ApplicantUpdateActionParameters", format=format_ts_candidate
            ),
            origin=TalentSoftProfileWarehouse.with_fixed_read_parameters(
                token_scope="MatchingIndexation"
            ),
            target=HrFlowProfileWarehouse.with_fixed_write_parameters(
                edit=True, only_edit_fields=["tags"]
            ),
        ),
    ],
)
