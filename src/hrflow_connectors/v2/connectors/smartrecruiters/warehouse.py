from hrflow_connectors.v2.connectors.smartrecruiters.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

SmartRecruitersWarehouse = Warehouse(
    auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle)
)
