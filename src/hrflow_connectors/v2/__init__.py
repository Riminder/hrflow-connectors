from hrflow_connectors.v2.connectors.admen import Admen
from hrflow_connectors.v2.connectors.adzuna import Adzuna
from hrflow_connectors.v2.connectors.breezyhr import BreezyHR
from hrflow_connectors.v2.connectors.bullhorn import Bullhorn
from hrflow_connectors.v2.connectors.ceipal import Ceipal
from hrflow_connectors.v2.connectors.flatchr import Flatchr
from hrflow_connectors.v2.connectors.francetravail import FranceTravail
from hrflow_connectors.v2.connectors.greenhouse import Greenhouse
from hrflow_connectors.v2.connectors.hubspot import Hubspot
from hrflow_connectors.v2.connectors.jazzhr import JazzHR
from hrflow_connectors.v2.connectors.recruitee import Recruitee
from hrflow_connectors.v2.connectors.smartrecruiters import SmartRecruiters
from hrflow_connectors.v2.connectors.taleez import Taleez
from hrflow_connectors.v2.connectors.teamtailor import Teamtailor
from hrflow_connectors.v2.connectors.waalaxy import Waalaxy
from hrflow_connectors.v2.connectors.workable import Workable
from hrflow_connectors.v2.connectors.zohorecruit import ZohoRecruit
from hrflow_connectors.v2.core.connector import (  # noqa: F401
    hrflow_connectors_manifest as hrflow_connectors_manifest,
)
from hrflow_connectors.v2.core.documentation import (  # noqa: F401
    hrflow_connectors_docs as hrflow_connectors_docs,
)

__CONNECTORS__ = [
    Bullhorn,
    ZohoRecruit,
    Admen,
    SmartRecruiters,
    Hubspot,
    Recruitee,
    Adzuna,
    FranceTravail,
    Flatchr,
    BreezyHR,
    Teamtailor,
    Taleez,
    Workable,
    Waalaxy,
    Greenhouse,
    JazzHR,
    Ceipal,
]
