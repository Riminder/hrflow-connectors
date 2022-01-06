from typing import Iterator, Dict, Any
from pydantic import Field
from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....utils.logger import get_logger
from ....utils.clean_text import remove_html_tags
from ....core.auth import OAuth2PasswordCredentialsBody, AuthorizationAuth
logger = get_logger()


class PullJobs(HTTPStream, BoardAction):
    auth: AuthorizationAuth
    top: int= Field(20, description="show ony the first n items, value by default = `20`")
    skip: int= Field(0, description="Skip the first n items")
    api_server: str= Field(..., description="the API server for your company from the list of API servers for SAP SuccessFactors data centers")

    @property
    def base_url(self):
        return "https://{}/odata/v2/JobRequisitionLocale?%24skip={}&%24top={}".format(self.api_server, self.skip, self.top)

    @property
    def http_method(self):
        return "GET"
