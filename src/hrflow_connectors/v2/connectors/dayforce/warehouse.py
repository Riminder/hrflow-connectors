from hrflow_connectors.v2.connectors.dayforce.aisles import AuthParameters, JobsAisle
from hrflow_connectors.v2.core.warehouse import Warehouse

DayforceWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle,))
