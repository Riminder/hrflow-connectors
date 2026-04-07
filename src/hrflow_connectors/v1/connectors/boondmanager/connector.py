import typing as t

from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)
from hrflow_connectors.v1.connectors.boondmanager.warehouse import (
    BoondManagerCandidateParsingWarehouse,
    BoondManagerCandidateWarehouse,
    BoondManagerOpportunityWarehouse,
)
from hrflow_connectors.v1.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileParsingWarehouse,
    HrFlowProfileWarehouse,
)


def get_skills_and_tasks(
    skills_str: t.Optional[str],
) -> t.Tuple[t.List[dict], t.List[dict]]:
    if not skills_str:
        return [], []
    skills, tasks = [], []
    for token in skills_str.split(","):
        token = token.strip()
        if not token:
            continue
        if len(token) > 15:
            tasks.append(dict(name=token, value=None))
        else:
            skills.append(dict(name=token, type=None, value=None))
    return skills, tasks


def get_languages(languages: t.Optional[t.List[dict]]) -> t.List[dict]:
    if not languages:
        return []
    return [
        dict(name=(lang.get("language") or "").strip(), value=None)
        for lang in languages
        if lang.get("language")
    ]


def get_education(diplomas: t.Optional[t.List[str]]) -> t.List[dict]:
    if not diplomas:
        return []
    educations = []
    for edu_str in diplomas:
        parts = edu_str.split(" - ")
        if not parts:
            continue
        title = end_year = institution = None
        if parts[0].strip().isdigit():
            end_year = parts[0].strip()
            title = parts[1].strip() if len(parts) > 1 else None
            institution = parts[2].strip() if len(parts) > 2 else None
        else:
            title = parts[0].strip()
            if len(parts) > 1 and parts[1].strip()[:4].isdigit():
                end_year = parts[1].strip()[:4]
                institution = parts[2].strip() if len(parts) > 2 else None
            else:
                institution = parts[1].strip() if len(parts) > 1 else None
        educations.append(
            dict(
                title=title,
                school=institution,
                date_start=None,
                date_end=end_year,
                description=edu_str,
                location=dict(text=None, lat=None, lng=None),
            )
        )
    return educations


def get_experience(references: t.Optional[t.List[dict]]) -> t.List[dict]:
    if not references:
        return []
    sorted_refs = sorted(references, key=lambda r: r.get("row", 0))
    experiences = []
    for ref in sorted_refs:
        start_year = ref.get("startYear")
        start_month = ref.get("startMonth")
        end_year = ref.get("endYear")
        end_month = ref.get("endMonth")
        date_start = (
            f"{start_year}-{start_month}-01" if start_year and start_month else None
        )
        date_end = f"{end_year}-{end_month}-01" if end_year and end_month else None
        exp_skills, exp_tasks = get_skills_and_tasks(ref.get("skills", "") or "")
        experiences.append(
            dict(
                title=ref.get("title"),
                company=ref.get("company"),
                date_start=date_start,
                date_end=date_end,
                description=ref.get("description"),
                location=dict(text=ref.get("location"), lat=None, lng=None),
                skills=exp_skills,
                tasks=exp_tasks,
            )
        )
    return experiences


def format_opportunity(boondmanager_opportunity: dict) -> dict:
    attributes = boondmanager_opportunity.get("attributes", {})
    app_dictionary = boondmanager_opportunity.get("_app_dictionary", {})
    setting = (app_dictionary.get("data") or {}).get("setting", {})

    mob_opts = [
        option
        for item in setting.get("mobilityArea", [])
        for option in item.get("option", [])
    ]
    act_opts = [
        option
        for item in setting.get("activityArea", [])
        for option in item.get("option", [])
    ]

    place_id = attributes.get("place")
    expertise_area_id = attributes.get("expertiseArea")
    origin_id = (attributes.get("origin") or {}).get("typeOf")
    duration_id = attributes.get("duration")

    state_id = attributes.get("state")
    type_of_id = attributes.get("typeOf")
    mode_id = attributes.get("mode")

    tags = [
        dict(name="boondmanager_reference", value=attributes.get("reference")),
        dict(name="boondmanager_state_id", value=state_id),
        dict(
            name="boondmanager_state_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("state", {}).get("opportunity", [])
                    if i.get("id") == state_id
                ),
                None,
            ),
        ),
        dict(name="boondmanager_typeof_id", value=type_of_id),
        dict(
            name="boondmanager_typeof_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("typeOf", {}).get("opportunity", [])
                    if i.get("id") == type_of_id
                ),
                None,
            ),
        ),
        dict(name="boondmanager_mode_id", value=mode_id),
        dict(
            name="boondmanager_mode_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("workUnitRate", [])
                    if i.get("id") == mode_id
                ),
                None,
            ),
        ),
        dict(name="boondmanager_expertise_area_id", value=expertise_area_id),
        dict(
            name="boondmanager_expertise_area_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("expertiseArea", [])
                    if i.get("id") == expertise_area_id
                ),
                None,
            ),
        ),
        dict(name="boondmanager_origin_id", value=origin_id),
        dict(
            name="boondmanager_origin_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("origin", [])
                    if i.get("id") == origin_id
                ),
                None,
            ),
        ),
        dict(name="boondmanager_duration_id", value=duration_id),
        dict(
            name="boondmanager_duration_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("duration", [])
                    if i.get("id") == duration_id
                ),
                None,
            ),
        ),
        dict(name="boondmanager_start_date", value=attributes.get("startDate")),
        dict(name="boondmanager_end_date", value=attributes.get("endDate")),
        dict(
            name="boondmanager_number_of_active_positionings",
            value=attributes.get("numberOfActivePositionings"),
        ),
    ]
    for area_id in attributes.get("activityAreas") or []:
        tags.append(dict(name="boondmanager_activity_area_id", value=area_id))
        tags.append(
            dict(
                name="boondmanager_activity_area_value",
                value=next(
                    (o["value"] for o in act_opts if o.get("id") == area_id), None
                ),
            )
        )

    sections = [
        dict(
            name="boondmanager_description",
            title="Description",
            description=attributes.get("description"),
        ),
    ]
    if attributes.get("criteria"):
        sections.append(
            dict(
                name="boondmanager_criteria",
                title="Criteria",
                description=attributes.get("criteria"),
            )
        )

    skills = [
        dict(name=tool, type="hard", value=None)
        for tool in (attributes.get("tools") or [])
        if tool
    ]

    return dict(
        reference=str(boondmanager_opportunity.get("id", "")),
        name=attributes.get("title"),
        created_at=attributes.get("creationDate"),
        updated_at=attributes.get("updateDate"),
        location=dict(
            text=next(
                (o["value"] for o in mob_opts if o.get("id") == place_id),
                str(place_id) if place_id is not None else None,
            ),
            lat=None,
            lng=None,
        ),
        sections=sections,
        skills=skills,
        tags=tags,
    )


def format_candidate(boondmanager_candidate: dict) -> dict:
    attributes = boondmanager_candidate.get("attributes", {})
    app_dictionary = boondmanager_candidate.get("_app_dictionary", {})
    setting = (app_dictionary.get("data") or {}).get("setting", {})

    mob_opts = [
        option
        for item in setting.get("mobilityArea", [])
        for option in item.get("option", [])
    ]
    act_opts = [
        option
        for item in setting.get("activityArea", [])
        for option in item.get("option", [])
    ]

    skills, tasks = get_skills_and_tasks(attributes.get("skills"))

    state_id = attributes.get("state")
    availability_id = attributes.get("availability")
    global_eval_id = attributes.get("globalEvaluation")

    tags = [
        dict(name="boondmanager_state_id", value=state_id),
        dict(
            name="boondmanager_state_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("state", {}).get("candidate", [])
                    if i.get("id") == state_id
                ),
                None,
            ),
        ),
        dict(name="boondmanager_availability_id", value=availability_id),
        dict(
            name="boondmanager_availability_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("availability", [])
                    if i.get("id") == availability_id
                ),
                None,
            ),
        ),
        dict(name="boondmanager_global_evaluation_id", value=global_eval_id),
        dict(
            name="boondmanager_global_evaluation_value",
            value=next(
                (
                    i["value"]
                    for i in setting.get("evaluation", [])
                    if i.get("id") == global_eval_id
                ),
                None,
            ),
        ),
        dict(
            name="boondmanager_number_of_active_positionings",
            value=attributes.get("numberOfActivePositionings"),
        ),
    ]
    for area_id in attributes.get("expertiseAreas") or []:
        tags.append(dict(name="boondmanager_expertise_area_id", value=area_id))
        tags.append(
            dict(
                name="boondmanager_expertise_area_value",
                value=next(
                    (
                        i["value"]
                        for i in setting.get("expertiseArea", [])
                        if i.get("id") == area_id
                    ),
                    None,
                ),
            )
        )
    for area_id in attributes.get("activityAreas") or []:
        tags.append(dict(name="boondmanager_activity_area_id", value=area_id))
        tags.append(
            dict(
                name="boondmanager_activity_area_value",
                value=next(
                    (o["value"] for o in act_opts if o.get("id") == area_id), None
                ),
            )
        )
    for area_id in attributes.get("mobilityAreas") or []:
        tags.append(dict(name="boondmanager_mobility_area_id", value=area_id))
        tags.append(
            dict(
                name="boondmanager_mobility_area_value",
                value=next(
                    (o["value"] for o in mob_opts if o.get("id") == area_id), None
                ),
            )
        )

    return dict(
        reference=str(boondmanager_candidate.get("id", "")),
        created_at=attributes.get("creationDate"),
        updated_at=attributes.get("updateDate"),
        info=dict(
            first_name=attributes.get("firstName"),
            last_name=attributes.get("lastName"),
            full_name=" ".join(
                filter(None, [attributes.get("firstName"), attributes.get("lastName")])
            ),
            date_birth=attributes.get("dateOfBirth"),
            email=attributes.get("email1"),
            phone=attributes.get("phone1"),
            urls=[
                dict(type=sn.get("network"), url=sn.get("url"))
                for sn in (attributes.get("socialNetworks") or [])
            ],
            location=dict(
                text=attributes.get("address"),
                lat=None,
                lng=None,
                fields=dict(
                    text=attributes.get("address"),
                    postcode=attributes.get("postcode"),
                    city=attributes.get("town"),
                    country=attributes.get("country"),
                ),
            ),
            summary=attributes.get("title"),
        ),
        skills=skills,
        tasks=tasks,
        languages=get_languages(attributes.get("languages")),
        experiences=get_experience(attributes.get("references")),
        educations=get_education(attributes.get("diplomas")),
        tags=tags,
    )


def format_parsing_candidate(boondmanager_candidate: dict) -> dict:
    return {
        "reference": str(boondmanager_candidate.get("id", "")),
        "created_at": boondmanager_candidate.get("attributes", {}).get("creationDate"),
        "tags": [],
        "metadatas": [],
        "resume": {
            "raw": boondmanager_candidate.get("_resume_bytes"),
            "content_type": "application/pdf",
        },
    }


BoondManager = Connector(
    name="BoondManager",
    type=ConnectorType.Other,
    subtype="boondmanager",
    description=(
        "BoondManager is a French ERP platform for consulting and staffing firms."
        " It centralises resource management, recruitment pipelines, commercial"
        " opportunities, and project tracking in a single tool."
    ),
    url="https://www.boondmanager.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all opportunities (job openings) from BoondManager"
                " and sends them to an HrFlow.ai Board."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadOpportunitiesActionParameters",
                format=format_opportunity,
            ),
            origin=BoondManagerOpportunityWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all candidates from BoondManager"
                " and sends them to an HrFlow.ai Source."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadCandidatesActionParameters",
                format=format_candidate,
            ),
            origin=BoondManagerCandidateWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.pull_resume_attachment_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves candidate resumes from BoondManager"
                " and parses them using the HrFlow.ai parsing engine."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadCandidatesParsingActionParameters",
                format=format_parsing_candidate,
            ),
            origin=BoondManagerCandidateParsingWarehouse,
            target=HrFlowProfileParsingWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
