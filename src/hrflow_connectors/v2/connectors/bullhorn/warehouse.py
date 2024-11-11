from hrflow_connectors.v2.core.warehouse import Warehouse
from hrflow_connectors.v2.connectors.bullhorn.aisles import (
    JobsAisle,
    ProfilesAisle,
    ApplicationsAisle,
    AuthParameters,
)

BullhornWarehouse = Warehouse(
    auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle, ApplicationsAisle)
)
