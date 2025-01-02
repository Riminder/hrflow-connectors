from hrflow_connectors.v2.connectors.waalaxy.aisles import AuthParameters, ProfilesAisle
from hrflow_connectors.v2.core.warehouse import Warehouse

WaalaxyWarehouse = Warehouse(auth=AuthParameters, aisles=(ProfilesAisle,))
