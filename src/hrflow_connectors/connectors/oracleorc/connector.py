import typing as t

from hrflow_connectors.connectors.hrflow.schemas import HrFlowJob, HrFlowProfile
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.oracleorc.schemas import OracleORCCandidate
from hrflow_connectors.connectors.oracleorc.utils.tools import (
    _hrflow_job_assign_location,
    _hrflow_job_assign_name,
    _hrflow_job_assign_ranges_date,
    _hrflow_object_assign_metadatas_and_tags,
    _hrflow_profile_assign_info,
    _oracleorc_profile_assign_attachments,
    _oracleorc_profile_assign_educations,
    _oracleorc_profile_assign_experiences,
    _oracleorc_profile_assign_info,
    _oracleorc_profile_assign_skills,
)
from hrflow_connectors.connectors.oracleorc.warehouse import (
    OracleORCJobsWarehouse,
    OracleORCProfilesWarehouse,
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

_ORACLEORC_HCM_URL = "https://www.oracle.com/human-capital-management/recruiting/"
_ORACLEORC_HCM_DESCRIPTION = (
    "Oracle Fusion Cloud Recruiting, Oracle's native and innovative hiring solution, is"
    " built to provide better candidate experiences, drive internal mobility, improve"
    " recruiter efficiency, and unify recruiting with the rest of the business. With"
    " the release of Oracle Fusion Cloud Recruiting Booster, Oracle now provides"
    " customers with an even more robust and comprehensive talent acquisition platform"
    " that delivers consistent experiences and operates seamlessly with Oracle"
    " Recruiting and Oracle Cloud HCM."
)


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    """
    Formats a `HrFlow.Profile` into a `OracleORC.Candidate`

    Returns:
      dict representing the formatted object
    """

    oracleorc_profile = OracleORCCandidate(
        CreationDate=hrflow_profile["created_at"],
        CandLastModifiedDate=hrflow_profile["updated_at"],
    )

    _oracleorc_profile_assign_info(oracleorc_profile, hrflow_profile)
    _oracleorc_profile_assign_attachments(oracleorc_profile, hrflow_profile)
    _oracleorc_profile_assign_educations(oracleorc_profile, hrflow_profile)
    _oracleorc_profile_assign_experiences(oracleorc_profile, hrflow_profile)
    _oracleorc_profile_assign_skills(oracleorc_profile, hrflow_profile)

    return oracleorc_profile.dict(exclude_none=True)


def format_oracleorc_job(oracleorc_job: t.Dict) -> t.Dict:
    """
    Formats an `OracleORC.JobRequisition` into a `HrFlow.Job`

    Returns:
      dict representing the formatted object
    """

    hrflow_job = HrFlowJob()

    _hrflow_job_assign_name(hrflow_job, oracleorc_job)
    _hrflow_job_assign_location(hrflow_job, oracleorc_job)
    _hrflow_job_assign_ranges_date(hrflow_job, oracleorc_job)
    _hrflow_object_assign_metadatas_and_tags(
        hrflow_job,
        oracleorc_job,
        mkeys=[
            "CorrectedKeyword",
            "Facets",
            "Keyword",
            "LastSelectedFacet",
            "Limit",
            "Offset",
            "Radius",
            "RadiusUnit",
            "SelectedCategoriesFacet",
            "SelectedFlexFieldsFacets",
            "SelectedLocationsFacet",
            "SelectedOrganizationsFacet",
            "SelectedPostingDatesFacet",
            "SelectedTitlesFacet",
            "SelectedWorkLocationsFacet",
            "SelectedWorkplaceTypesFacet",
            "TotalJobsCount",
            "UserTargetFacetInputTerm",
            "UserTargetFacetName",
        ],
        tkeys=[
            "BotQRShortCode",
            "CandidateNumber",
            "HotJobFlag",
            "JobFamilyId",
            "Keyword",
            "LocationId",
            "OrganizationId",
            "RequisitionId",
            "SearchId",
            "SiteNumber",
            "SortBy",
            "UseExactKeywordFlag",
        ],
    )

    return hrflow_job.dict()


def format_oracleorc_profile(oracleorc_profile: t.Dict) -> t.Dict:
    """
    Formats an `OracleORC.Candidate` into a `HrFlow.Profile`

    Returns:
      dict representing the formatted object
    """

    hrflow_profile = HrFlowProfile(
        created_at=oracleorc_profile["CreationDate"],
        updated_at=oracleorc_profile["LastUpdateDate"],
    )

    _hrflow_profile_assign_info(hrflow_profile, oracleorc_profile)
    _hrflow_object_assign_metadatas_and_tags(
        hrflow_profile,
        oracleorc_profile,
        mkeys=[
            "CompaignOptIn",
            "DisplayName",
            "Honors",
            "KnownAs",
            "ListName",
            "MiddleNames",
            "PreNameAdjunct",
            "PreferredLanguage",
            "PreviousLastName",
            "SourceMedium",
            "Suffix",
            "Title",
        ],
        tkeys=[
            "CandidateNumber",
            "CandidateType",
            "CreatedBy",
            "LastUpdatedBy",
            "MilitaryRank",
            "PersonId",
            "SourceName",
        ],
    )

    return hrflow_profile.dict()


OracleORC = Connector(
    name="OracleORC",
    type=ConnectorType.HCM,
    description=_ORACLEORC_HCM_DESCRIPTION,
    url=_ORACLEORC_HCM_URL,
    actions=[
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            action_type=ActionType.outbound,
            origin=HrFlowProfileWarehouse,
            target=OracleORCProfilesWarehouse,
            description=(
                "Retrive Profiles from HrFlow and create a candidate on Oracle ORC for"
                " a Job Requisition"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_hrflow_profile
            ),
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            action_type=ActionType.inbound,
            origin=OracleORCProfilesWarehouse,
            target=HrFlowProfileWarehouse,
            description="Retrive Candidates from Oracle ORC into a HrFLow source",
            parameters=BaseActionParameters.with_defaults(
                "ReadProfilesActionParameters", format=format_oracleorc_profile
            ),
        ),
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            action_type=ActionType.inbound,
            origin=OracleORCJobsWarehouse,
            target=HrFlowJobWarehouse,
            description="Retrive Job Requisitions from Oracle ORC into a HrFlow board",
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_oracleorc_job
            ),
        ),
    ],
)
