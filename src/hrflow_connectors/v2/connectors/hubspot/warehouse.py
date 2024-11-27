from hrflow_connectors.v2.connectors.hubspot.aisles import AuthParameters, ProfilesAisle
from hrflow_connectors.v2.core.warehouse import Warehouse

HubspotWarehouse = Warehouse(auth=AuthParameters, aisles=(ProfilesAisle,))
