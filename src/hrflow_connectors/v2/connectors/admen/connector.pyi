# This file is generated automatically
from hrflow_connectors.v2.core.connector import Connector, PublicActionInterface

class AdmenProto(Connector):
    create_jobs_in_hrflow: PublicActionInterface
    update_jobs_in_hrflow: PublicActionInterface
    archive_jobs_in_hrflow: PublicActionInterface
    create_profiles_in_hrflow: PublicActionInterface
    update_profiles_in_hrflow: PublicActionInterface
    archive_profiles_in_hrflow: PublicActionInterface
    create_profiles_in_admen: PublicActionInterface
    update_profiles_in_admen: PublicActionInterface
    archive_profiles_in_admen: PublicActionInterface
    
Admen: AdmenProto