# This file is generated automatically
from hrflow_connectors.v2.core.connector import Connector, PublicActionInterface

class JobologyProto(Connector):
    create_profiles_in_hrflow: PublicActionInterface
    update_profiles_in_hrflow: PublicActionInterface
    archive_profiles_in_hrflow: PublicActionInterface
    
Jobology: JobologyProto