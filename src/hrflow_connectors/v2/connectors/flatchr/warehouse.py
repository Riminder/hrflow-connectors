from hrflow_connectors.v2.connectors.flatchr.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

FlatchrWarehouse = Warehouse(
    auth=AuthParameters,
    aisles=(JobsAisle, ProfilesAisle),
)
