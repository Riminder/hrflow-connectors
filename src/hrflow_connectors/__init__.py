from hrflow_connectors.core import backend
from hrflow_connectors.v1.connectors.adzuna.connector import Adzuna
from hrflow_connectors.v1.connectors.breezyhr import BreezyHR
from hrflow_connectors.v1.connectors.bullhorn import Bullhorn
from hrflow_connectors.v1.connectors.carrevolutis import Carrevolutis
from hrflow_connectors.v1.connectors.ceridian import Ceridian
from hrflow_connectors.v1.connectors.digitalrecruiters import DigitalRecruiters
from hrflow_connectors.v1.connectors.greenhouse.connector import Greenhouse
from hrflow_connectors.v1.connectors.hubspot import Hubspot
from hrflow_connectors.v1.connectors.jobology import Jobology
from hrflow_connectors.v1.connectors.lever import Lever
from hrflow_connectors.v1.connectors.meteojob import Meteojob
from hrflow_connectors.v1.connectors.poleemploi import PoleEmploi
from hrflow_connectors.v1.connectors.recruitee import Recruitee
from hrflow_connectors.v1.connectors.salesforce import Salesforce
from hrflow_connectors.v1.connectors.sapsuccessfactors import SAPSuccessFactors
from hrflow_connectors.v1.connectors.smartrecruiters import SmartRecruiters
from hrflow_connectors.v1.connectors.taleez.connector import Taleez
from hrflow_connectors.v1.connectors.talentsoft import TalentSoft
from hrflow_connectors.v1.connectors.teamtailor import Teamtailor
from hrflow_connectors.v1.connectors.waalaxy import Waalaxy
from hrflow_connectors.v1.connectors.workable import Workable
from hrflow_connectors.v1.core.connector import hrflow_connectors_manifest  # noqa
from hrflow_connectors.v1.core.documentation import generate_docs  # noqa

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
    DigitalRecruiters,
    Jobology,
    Meteojob,
    Carrevolutis,
]

backend.configure_store()
