from ....core.auth import OAuth2PasswordCredentialsBody
from ....core.http import HTTPAction


class GetAllJobs(HTTPAction):
    auth: OAuth2PasswordCredentialsBody
    url: str = "https://vulcain-eng--recette.my.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/"
    http_method: str = "GET"
