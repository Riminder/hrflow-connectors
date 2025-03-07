from hrflow_connectors.v2.connectors.jazzhr.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

JazzhrWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
