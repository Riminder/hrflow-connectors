from hrflow_connectors.v2.core.hrflow.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

HrFlowWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
