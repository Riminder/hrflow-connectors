from hrflow_connectors.v2.connectors.breezyhr.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

BreezyHrWarehouse = Warehouse(
    auth=AuthParameters,
    aisles=(
        JobsAisle,
        ProfilesAisle,
    ),
)
