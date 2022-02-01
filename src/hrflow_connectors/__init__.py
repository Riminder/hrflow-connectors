from hrflow_connectors.connectors.smartrecruiters import SmartRecruiters
from hrflow_connectors.core.connector import hrflow_connectors_manifest  # noqa

__CONNECTORS__ = [SmartRecruiters]

# This makes sure that connector are in module namespace
# and that the automatic workflow code generation should work
for connector in __CONNECTORS__:
    globals()[connector.model.name] = connector
