from hrflow_connectors.connectors.adzuna import Adzuna
from hrflow_connectors.connectors.breezyhr import BreezyHR
from hrflow_connectors.connectors.bullhorn import Bullhorn
from hrflow_connectors.connectors.carrevolutis import Carrevolutis
from hrflow_connectors.connectors.ceridian import Ceridian
from hrflow_connectors.connectors.digitalrecruiters import DigitalRecruiters
from hrflow_connectors.connectors.greenhouse.connector import Greenhouse
from hrflow_connectors.connectors.hubspot import Hubspot
from hrflow_connectors.connectors.jobology import Jobology
from hrflow_connectors.connectors.lever import Lever
from hrflow_connectors.connectors.meteojob import Meteojob
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
from hrflow_connectors.core.connector_v2 import (  # noqa
    hrflow_connectors_manifest as hrflow_connectors_manifest_v2,
)
from hrflow_connectors.core.documentation import generate_docs  # noqa
from hrflow_connectors.core.documentation_v2 import generate_docs_v2  # noqa

__CONNECTORS__V1__ = [
    Adzuna,
    BreezyHR,
    Carrevolutis,
    Ceridian,
    DigitalRecruiters,
    Greenhouse,
    Hubspot,
    Jobology,
    Lever,
    Meteojob,
    PoleEmploi,
    Recruitee,
    Salesforce,
    SAPSuccessFactors,
    SmartRecruiters,
    Taleez,
    TalentSoft,
    Teamtailor,
    Waalaxy,
    Workable,
]

__CONNECTORS__V2__ = [
    Bullhorn,
]


backend.configure_store()
