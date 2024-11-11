from hrflow_connectors.v2.core.warehouse import Warehouse
from hrflow_connectors.v2.core.hrflow.aisles import (
    JobsAisle,
    ProfilesAisle,
    AuthParameters,
)


HrFlowWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
