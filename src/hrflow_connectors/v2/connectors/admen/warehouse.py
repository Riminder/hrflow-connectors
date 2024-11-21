from hrflow_connectors.v2.connectors.admen.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

AdmenWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
