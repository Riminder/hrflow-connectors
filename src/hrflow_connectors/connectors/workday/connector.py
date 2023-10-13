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
from hrflow_connectors.connectors.workday.utils.tools import (
    _workday_job_location_get,
    _workday_job_metadatas_get,
    _workday_job_tags_get,
    _hrflow_profile_candidate_get,
    _hrflow_profile_educations_get,
    _hrflow_profile_experiences_get,
    _hrflow_profile_extracted_skills_get,
    _hrflow_profile_languages_get,
    _hrflow_profile_resume_get,
    _hrflow_profile_skills_get,
    _hrflow_profile_tags_get,
    _workday_ranges_date_get,
)
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.workday.warehouse import (
    WorkdayJobsWarehouse,
    WorkdayProfilesWarehouse,
)

_WORKDAY_HCM_URL = (
    "https://www.workday.com/en-us/products/human-capital-management/overview.html"
)
_WORKDAY_DESCRIPTION = (
    "Manage the full talent acquisition lifecycle. With recruiting, engagement, and"
    " Workday Skills Cloud unified with Workday Human Capital Management (HCM), we've"
    " got you covered every step of the way. Our skills intelligence foundation helps"
    " you build diverse teams by expanding candidate pools with equitable, AI- and"
    " ML-driven job recommendations."
)


def _format_workday_job(workday_job: t.Dict) -> t.Dict:
    hrflow_job = dict(
        name=workday_job["title"],
        url=workday_job["url"],
        summary=workday_job["jobDescription"],
        location=_workday_job_location_get(workday_job["primaryLocation"]),
        sections=dict(
            name="Description",
            title="Description",
            description=workday_job["jobDescription"],
        ),
        metadatas=_workday_job_metadatas_get(workday_job),
        ranges_date=_workday_ranges_date_get(workday_job),
        tags=_workday_job_tags_get(workday_job),
    )
    return hrflow_job


def _format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    workday_profile = dict(
        candidateTags=_hrflow_profile_tags_get(hrflow_profile),
        educations=_hrflow_profile_educations_get(hrflow_profile),
        experiences=_hrflow_profile_experiences_get(hrflow_profile),
        languages=_hrflow_profile_languages_get(hrflow_profile),
        tags=_hrflow_profile_tags_get(hrflow_profile),
        info=_hrflow_profile_candidate_get(hrflow_profile),
        skills=_hrflow_profile_skills_get(hrflow_profile),
        resume=_hrflow_profile_resume_get(hrflow_profile),
    )
    workday_profile.skills.extend(
        _hrflow_profile_extracted_skills_get(hrflow_profile),
    )
    return workday_profile


Workday = Connector(
    name="Workday",
    type=ConnectorType.HCM,
    description=_WORKDAY_DESCRIPTION,
    url=_WORKDAY_HCM_URL,
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            action_type=ActionType.inbound,
            origin=WorkdayJobsWarehouse,
            target=HrFlowJobWarehouse,
            description="Retrive Jobs from Workday and index them to a HrFlow board.",
            parameters=BaseActionParameters.with_defaults(
                "WorkdayReadJobsParameters", format=_format_workday_job
            ),
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            action_type=ActionType.outbound,
            origin=HrFlowProfileWarehouse,
            target=WorkdayProfilesWarehouse,
            description="Retrive profile from HrFlow source and post them to Workday.",
            parameters=BaseActionParameters.with_defaults(
                "WorkdayWriteProfileParameters", format=_format_hrflow_profile
            ),
        ),
    ],
)
