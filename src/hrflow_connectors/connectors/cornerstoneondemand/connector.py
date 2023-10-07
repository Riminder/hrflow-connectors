import typing as t

from hrflow_connectors.connectors.cornerstoneondemand.schemas import (
    CornerstoneOnDemandApplicationPreferences,
    CornerstoneOnDemandCreateCandidate,
)
from hrflow_connectors.connectors.cornerstoneondemand.utils.tools import (
    _cornerstone_ondemand_general_entities_get,
    _cornerstone_ondemand_job_languages_get,
    _cornerstone_ondemand_job_location_get,
    _cornerstone_ondemand_job_ranges_date_get,
    _cornerstone_ondemand_job_ranges_float_get,
    _cornerstone_ondemand_job_sections_get,
    _cornerstone_ondemand_profile_info_get,
    _hrflow_profile_attachments_get,
    _hrflow_profile_candidate_get,
)
from hrflow_connectors.connectors.cornerstoneondemand.warehouse import (
    CornerstoneOnDemandJobsWarehouse,
    CornerstoneOnDemandProfilesWarehouse,
)
from hrflow_connectors.connectors.hrflow.schemas import HrFlowJob, HrFlowProfile
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
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

_CORNERSTONE_ONDEMAND_HCM_URL = "https://www.cornerstoneondemand.com/solutions/hcm/"
_CORNERSTONE_ONDEMAND_HCM_DESCRIPTION = (
    "Cornerstone OnDemand is a comprehensive Human Capital Management (HCM)"
    " solution designed to meet the needs of the modern workforce. Our platformempowers"
    " organizations to grow, engage, and retain their people, fostering agility in an"
    " ever-changing business landscape. With the help of AI-powered, skills-enabled"
    " talent management, Cornerstone OnDemand provides a single view of your"
    " people at every stage of their journey, helping you achieve your people-related"
    " goals."
)


def format_cornerstone_ondemand_job(cornerstone_job: t.Dict) -> t.Dict:
    """
    Formats a `CornerstoneOnDemand.Job` into a `HrFlow.Job`.

    Returns:
      Dictionary of the respective object
    """

    hrflow_job = HrFlowJob(
        name=cornerstone_job["Title"],
        reference=str(cornerstone_job["Id"]),
        location=_cornerstone_ondemand_job_location_get(cornerstone_job),
        sections=_cornerstone_ondemand_job_sections_get(cornerstone_job),
        url=cornerstone_job["DefaultURL"],
        summary=cornerstone_job["MinimumQualification"],
        created_at=cornerstone_job["DefaultEffectiveDate"],
        languages=_cornerstone_ondemand_job_languages_get(cornerstone_job),
        ranges_float=_cornerstone_ondemand_job_ranges_float_get(cornerstone_job),
        ranges_date=_cornerstone_ondemand_job_ranges_date_get(cornerstone_job),
        metadatas=_cornerstone_ondemand_general_entities_get(
            cornerstone_job,
            names=[
                "ApplicantCount",
                "CreateDateLocal",
                "Compensation",
                "ContactPhone",
                "CurrencySymbol",
                "DaysOpen",
                "ExternalDescription",
                "IdealQualification",
                "InternalDescription",
                "JobResponsibilities",
                "LastModificationDate",
                "Keywords",
                "OpenPostingCount",
                "Openings",
                "ReferalBonus",
                "SuggestedReferralCount",
                "TargetHireDate",
            ],
        ),
        tags=_cornerstone_ondemand_general_entities_get(
            cornerstone_job,
            names=[
                "CanAppy",
                "Division",
                "EEOCategory",
                "EmploymentStatus",
                "EmploymentType",
                "Ongoing",
                "Priority",
                "Status",
            ],
            prefix=True,
        ),
    )

    return hrflow_job.dict(exclude_none=True)


def format_cornerstone_ondemand_profile(cornerstone_profile: t.Dict) -> t.Dict:
    """
    Formats a `CornerstoneOnDemand.Candidate` into a `HrFlow.Profile`.

    Returns:
      Dictionary of the respective object
    """

    hrflow_profile = HrFlowProfile(
        reference=cornerstone_profile["Id"],
        created_at=cornerstone_profile["ApplicationReceivedDateLocal"],
        info=_cornerstone_ondemand_profile_info_get(cornerstone_profile),
        metadatas=_cornerstone_ondemand_general_entities_get(
            cornerstone_profile,
            names=[
                "AverageRating",
                "Fax",
                "HomePhone",
                "PositionTitle",
                "PreviousStatus",
                "Username",
            ],
        ),
        tags=_cornerstone_ondemand_general_entities_get(
            cornerstone_profile,
            names=[
                "CandidateType",
                "ConsiderForOtherJobs",
                "Ethnicity",
                "Source",
                "Status",
            ],
            prefix=True,
        ),
    )

    return hrflow_profile.dict(exclude_none=True)


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    """
    Formats a `HrFlow.Profile` into a `CornerstoneOnDemand.Candidate`.

    Returns:
      Dictionary of the respective object
    """

    cornerstone_profile = CornerstoneOnDemandCreateCandidate(
        applicationPreferences=CornerstoneOnDemandApplicationPreferences(),
        candidate=_hrflow_profile_candidate_get(hrflow_profile),
    )

    attachments = _hrflow_profile_attachments_get(hrflow_profile)
    if attachments is not None:
        cornerstone_profile.additionalAttachments = attachments.get("additionals")
        cornerstone_profile.resume = attachments.get("resume")
        cornerstone_profile.coverLetter = attachments.get("cover")

    # TODO: potentially need to decode attachments' content
    # it causes json serialization problems

    return cornerstone_profile.dict(exclude_none=True)


CornerstoneOnDemand = Connector(
    name="CornerstoneOnDemand",
    type=ConnectorType.HCM,
    description=_CORNERSTONE_ONDEMAND_HCM_DESCRIPTION,
    url=_CORNERSTONE_ONDEMAND_HCM_URL,
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            action_type=ActionType.inbound,
            origin=CornerstoneOnDemandJobsWarehouse,
            target=HrFlowJobWarehouse,
            description=(
                "Retrieve Job Requisitions from Cornerstone OnDemand and index them"
                " into a HrFlow board."
            ),
            parameters=BaseActionParameters.with_defaults(
                "CornerstoneOnDemandReadJobsActionParameters",
                format=format_cornerstone_ondemand_job,
            ),
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            action_type=ActionType.inbound,
            origin=CornerstoneOnDemandProfilesWarehouse,
            target=HrFlowProfileWarehouse,
            description=(
                "Retrieve Candidates from Cornerstone OnDemand and index them into a"
                " HrFlow source."
            ),
            parameters=BaseActionParameters.with_defaults(
                "CornerstoneOnDemandReadProfilesActionParameters",
                format=format_cornerstone_ondemand_profile,
            ),
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            action_type=ActionType.outbound,
            origin=HrFlowProfileWarehouse,
            target=CornerstoneOnDemandProfilesWarehouse,
            description=(
                "Retrieve Profile from HrFlow and candidate for a Cornerstone OnDemand"
                " Job Requisition."
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters",
                format=format_hrflow_profile,
            ),
        ),
    ],
)
