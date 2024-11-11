from hrflow_connectors.v2.core.warehouse import Warehouse

from .aisles import (
    AuthParameters,
    CandidatesAisle,
    LeadsAisle,
)

SmartLeadsWarehouse = Warehouse(
    auth=AuthParameters, aisles=(LeadsAisle, CandidatesAisle)
)
