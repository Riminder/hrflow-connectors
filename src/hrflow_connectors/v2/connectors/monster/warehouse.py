from hrflow_connectors.v2.connectors.monster.aisles import AuthParameters, ProfilesAisle
from hrflow_connectors.v2.core.warehouse import Warehouse

MonsterWarehouse = Warehouse(auth=AuthParameters, aisles=(ProfilesAisle,))
