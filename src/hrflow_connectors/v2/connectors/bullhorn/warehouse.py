from hrflow_connectors.v2.connectors.bullhorn.aisles import (
    ApplicationsAisle,
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

BullhornWarehouse = Warehouse(
    auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle, ApplicationsAisle)
)
