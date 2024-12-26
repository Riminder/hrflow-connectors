from hrflow_connectors.v2.connectors.workable.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

WorkableWarehouse = Warehouse(
    auth=AuthParameters,
    aisles=(
        JobsAisle,
        ProfilesAisle,
    ),
)
