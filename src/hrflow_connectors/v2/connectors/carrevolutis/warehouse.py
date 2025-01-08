from hrflow_connectors.v2.connectors.carrevolutis.aisles import (
    AuthParameters,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

CarrevolutisWarehouse = Warehouse(auth=AuthParameters, aisles=(ProfilesAisle,))
