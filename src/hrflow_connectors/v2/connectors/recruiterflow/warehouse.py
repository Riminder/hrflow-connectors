from hrflow_connectors.v2.connectors.recruiterflow.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

RecruiterFlowWarehouse = Warehouse(
    auth=AuthParameters,
    aisles=(
        JobsAisle,
        ProfilesAisle,
    ),
)
