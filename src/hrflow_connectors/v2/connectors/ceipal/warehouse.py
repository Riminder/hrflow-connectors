from hrflow_connectors.v2.connectors.ceipal.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

CeipalWarehouse = Warehouse(
    auth=AuthParameters,
    aisles=(
        JobsAisle,
        ProfilesAisle,
    ),
)
