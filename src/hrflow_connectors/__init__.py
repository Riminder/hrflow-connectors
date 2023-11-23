from hrflow_connectors.connectors.adzuna.connector import Adzuna
from hrflow_connectors.connectors.breezyhr import BreezyHR
from hrflow_connectors.connectors.bullhorn import Bullhorn
from hrflow_connectors.connectors.ceridian import Ceridian
from hrflow_connectors.connectors.greenhouse.connector import Greenhouse
from hrflow_connectors.connectors.hubspot import Hubspot
from hrflow_connectors.connectors.lever import Lever
from hrflow_connectors.connectors.poleemploi import PoleEmploi
from hrflow_connectors.connectors.recruitee import Recruitee
from hrflow_connectors.connectors.salesforce import Salesforce
from hrflow_connectors.connectors.sapsuccessfactors import SAPSuccessFactors
from hrflow_connectors.connectors.smartrecruiters import SmartRecruiters
from hrflow_connectors.connectors.taleez.connector import Taleez
from hrflow_connectors.connectors.talentsoft import TalentSoft
from hrflow_connectors.connectors.teamtailor import Teamtailor
from hrflow_connectors.connectors.waalaxy import Waalaxy
from hrflow_connectors.connectors.workable import Workable
from hrflow_connectors.core import backend
from hrflow_connectors.core.connector import hrflow_connectors_manifest  # noqa
from hrflow_connectors.core.documentation import generate_docs  # noqa

__version__ = "2.0.0"
__CONNECTORS__ = [
    SmartRecruiters,
    TalentSoft,
    PoleEmploi,
    Adzuna,
    Recruitee,
    Workable,
    BreezyHR,
    SAPSuccessFactors,
    Bullhorn,
    Ceridian,
    Greenhouse,
    Teamtailor,
    Waalaxy,
    Hubspot,
    Taleez,
    Lever,
    Salesforce,
]

# This makes sure that connector are in module namespace
# and that the automatic workflow code generation should work
for connector in __CONNECTORS__:
    globals()[connector.model.name] = connector

backend.configure_store()
