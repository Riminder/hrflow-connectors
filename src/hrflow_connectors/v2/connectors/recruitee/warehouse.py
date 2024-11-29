from hrflow_connectors.v2.connectors.recruitee.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

RecruiteeWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
