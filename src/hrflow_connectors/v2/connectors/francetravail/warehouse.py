from hrflow_connectors.v2.connectors.francetravail.aisles import (
    AuthParameters,
    JobsAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

FranceTravailWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle,))
