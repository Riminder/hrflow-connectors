import typing as t

from hrflow_connectors.connectors.hrflow.schemas import HrFlowJob
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.ukgpro.schemas import UKGProCandidate
from hrflow_connectors.connectors.ukgpro.utils.tools import (
    _hrflow_profile_contact_info_get,
    _hrflow_profile_educations_get,
    _hrflow_profile_experiences_get,
    _hrflow_profile_hyperlinks_get,
    _hrflow_profile_name_get,
    _select_translation,
    _ukgpro_job_locale_get,
    _ukgpro_job_location_get,
    _ukgpro_job_metadatas_get,
    _ukgpro_job_ranges_date_get,
    _ukgpro_job_ranges_float_get,
    _ukgpro_job_sections_get,
    _ukgpro_job_skills_get,
    _ukgpro_job_tags_get,
)
from hrflow_connectors.connectors.ukgpro.warehouse import (
    UKGProJobsWarehouse,
    UKGProProfilesWarehouse,
)
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)

_UKGPRO_HCM_URL = "https://www.ukg.com/solutions/ukg-pro-suite"
_UKGPRO_HCM_DESCRIPTION = (
    "Build a people-centered strategy with culture-driven HCM. Our comprehensive human"
    " capital management suite, designed for enterprise organizations, efficiently"
    " manages personnel data, handles robust payroll tasks, boosts scheduling"
    " efficiency, and helps attract, nurture, and grow your talent — all in one"
    " experience."
)


def format_ukgpro_job(ukgpro_job: t.Dict) -> t.Dict:
    """
    Formats an `UKGPro.Opportunity` as a `HrFlow.Job`

    Returns:
      Dictionary of the respective object
    """

    locale = _ukgpro_job_locale_get(ukgpro_job)

    hrflow_job = HrFlowJob(
        key=ukgpro_job["id"],
        reference=ukgpro_job["requisition_number"],
        created_at=ukgpro_job["created_at"],
        location=_ukgpro_job_location_get(ukgpro_job),
        sections=_ukgpro_job_sections_get(ukgpro_job, locale=locale),
        name=_select_translation(ukgpro_job["title"], locale),
        ranges_float=_ukgpro_job_ranges_float_get(ukgpro_job),
        ranges_date=_ukgpro_job_ranges_date_get(ukgpro_job),
        skills=_ukgpro_job_skills_get(ukgpro_job, locale),
        metadatas=_ukgpro_job_metadatas_get(ukgpro_job, locale),
        tags=_ukgpro_job_tags_get(ukgpro_job, locale),
    )

    return hrflow_job.dict(exclude_none=True)


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    """
    Formats an `UKGPro.Candidate` as a `HrFlow.Profile`

    Returns:
      Dictionary of the respective object
    """

    ukgpro_profile = UKGProCandidate(
        created_at=hrflow_profile["created_at"],
        name=_hrflow_profile_name_get(hrflow_profile),
        contact_info=_hrflow_profile_contact_info_get(hrflow_profile),
        hyperlinks=_hrflow_profile_hyperlinks_get(hrflow_profile),
        workexperience=_hrflow_profile_experiences_get(hrflow_profile),
        education=_hrflow_profile_educations_get(hrflow_profile),
    )

    return ukgpro_profile.dict(exclude_none=True)


UKGPro = Connector(
    name="UKGPro",
    type=ConnectorType.HCM,
    description=_UKGPRO_HCM_DESCRIPTION,
    url=_UKGPRO_HCM_URL,
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            action_type=ActionType.inbound,
            origin=UKGProJobsWarehouse,
            target=HrFlowJobWarehouse,
            description="Retrieve jobs from UKGPro and index them into a HrFlow board.",
            parameters=BaseActionParameters.with_defaults(
                "UKGProReadJobsParameters", format=format_ukgpro_job
            ),
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            action_type=ActionType.outbound,
            origin=HrFlowProfileWarehouse,
            target=UKGProProfilesWarehouse,
            description="Retrieve profiles from HrFlow and index them to UKGPro.",
            parameters=BaseActionParameters.with_defaults(
                "UKGProWriteProfileParameters", format=format_hrflow_profile
            ),
        ),
    ],
)
