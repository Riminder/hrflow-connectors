# This file is generated automatically
from hrflow_connectors.v2.core.connector import Connector, PublicActionInterface

class RecruiteeProto(Connector):
    create_profiles_in_hrflow: PublicActionInterface
    update_profiles_in_hrflow: PublicActionInterface
    archive_profiles_in_hrflow: PublicActionInterface
    create_profiles_in_recruitee: PublicActionInterface
    update_profiles_in_recruitee: PublicActionInterface
    archive_profiles_in_recruitee: PublicActionInterface
    create_jobs_in_hrflow: PublicActionInterface
    update_jobs_in_hrflow: PublicActionInterface
    archive_jobs_in_hrflow: PublicActionInterface
    
Recruitee: RecruiteeProto