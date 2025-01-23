from hrflow_connectors.v2.connectors.talentsoft.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

TalentSoftWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
