from hrflow_connectors.v2.connectors.zohorecruit.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

ZohoRecruitWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
