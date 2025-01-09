from hrflow_connectors.v2.connectors.salesforce.aisles import (
    AuthParameters,
    JobsAisle,
    ProfilesAisle,
)
from hrflow_connectors.v2.core.warehouse import Warehouse

SalesforceWarehouse = Warehouse(auth=AuthParameters, aisles=(JobsAisle, ProfilesAisle))
