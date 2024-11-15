from hrflow_connectors.v2.core.warehouse import Warehouse

from .aisles import ApplicationsAisle, AuthParameters, JobsAisle

HrFlowMiniWarehouse = Warehouse(
    auth=AuthParameters, aisles=(JobsAisle, ApplicationsAisle)
)
