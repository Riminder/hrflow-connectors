from hrflow_connectors.v2.connectors.teamtailor.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

TeamTailorWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
