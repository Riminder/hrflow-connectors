from hrflow_connectors.v2.connectors.jobijoba.aisles import (
    AuthParameters,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

JobijobaWarehouse = Warehouse(auth=AuthParameters, aisles=(ProfilesAisle,))
