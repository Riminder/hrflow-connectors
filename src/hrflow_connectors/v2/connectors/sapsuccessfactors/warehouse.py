from hrflow_connectors.v2.connectors.sapsuccessfactors.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

SAPWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
