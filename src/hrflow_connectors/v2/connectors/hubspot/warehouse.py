from hrflow_connectors.v2.connectors.hubspot.aisles import AuthParameters, ProfilesAisle
from hrflow_connectors.v2.core.warehouse import Warehouse

BullhornWarehouse = Warehouse(auth=AuthParameters, aisles=(ProfilesAisle))
