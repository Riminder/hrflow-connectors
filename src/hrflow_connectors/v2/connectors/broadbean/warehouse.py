from hrflow_connectors.v2.connectors.broadbean.aisles import (
    AuthParameters,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

BroadbeanWarehouse = Warehouse(
    auth=AuthParameters,
    aisles=(ProfilesAisle,),
)
