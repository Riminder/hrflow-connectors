from hrflow_connectors.v2.connectors.adzuna.aisles import AuthParameters, JobsAisle
from hrflow_connectors.v2.core.warehouse import Warehouse

AdzunaWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle,))
