from hrflow_connectors.v2.connectors.taleez.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

TaleezWarehouse = Warehouse(
    auth=AuthParameters,
    aisles=(
        JobsAisle,
        ProfilesAisle,
    ),
)
