from ....core.auth import OAuth2PasswordCredentialsBody
from ....core.http import HTTPAction


class GetAllJobs(HTTPAction):
    auth: OAuth2PasswordCredentialsBody

    @property
    def url_base(self):
        return "https://vulcain-eng--recette.my.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/"
    
    @property
    def http_method(self):
        return "GET"
