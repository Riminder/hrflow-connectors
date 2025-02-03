from hrflow_connectors.v2.connectors.homerun.aisles import AuthParameters, JobsAisle
from hrflow_connectors.v2.core.warehouse import Warehouse

HomerunWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle,))
