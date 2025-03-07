# This file is generated automatically
from hrflow_connectors.v2.core.connector import Connector, PublicActionInterface

class HubspotProto(Connector):
    create_profiles_in_hrflow: PublicActionInterface
    create_profiles_in_hubspot: PublicActionInterface
    update_profiles_in_hrflow: PublicActionInterface
    update_profiles_in_hubspot: PublicActionInterface
    archive_profiles_in_hrflow: PublicActionInterface
    archive_profiles_in_hubspot: PublicActionInterface
    
Hubspot: HubspotProto