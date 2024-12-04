# This file is generated automatically
from hrflow_connectors.v2.core.connector import Connector, PublicActionInterface

class AdzunaProto(Connector):
    create_jobs_in_hrflow: PublicActionInterface
    update_jobs_in_hrflow: PublicActionInterface
    archive_jobs_in_hrflow: PublicActionInterface
    
Adzuna: AdzunaProto