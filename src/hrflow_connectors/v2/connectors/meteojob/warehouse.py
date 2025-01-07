from hrflow_connectors.v2.connectors.meteojob.aisles import (
    AuthParameters,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

MeteojobWarehouse = Warehouse(auth=AuthParameters, aisles=(ProfilesAisle,))
