from hrflow_connectors.v2.connectors.greenhouse.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

GreenhouseWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
