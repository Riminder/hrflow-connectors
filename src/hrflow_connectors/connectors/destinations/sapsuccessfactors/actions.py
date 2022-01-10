from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from pydantic import Field
from typing import Dict, Any, Optional, Union, List


class PushProfile(ProfileDestinationAction, HTTPStream):

    auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]
    payload: Dict[str, Any] = dict()
    subdomain: str = Field(
        ...,
        description="the API server for your company from the list of API servers for SAP SuccessFactors data centers",
    )


    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"

    @property
    def base_url(self):
        return "https://{}/odata/v2/Candidate".format(self.subdomain)

    @property
    def http_method(self):
        return "GET"

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:

        profile = dict()
        

        return profile





    def push(self, data: Dict[str, Any]):
        """
        Push profile
        Args:
            data (Dict[str, Any]): Profile
        """
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to sapsuccesfactors api-server: {} failed : `{}`".format(self.subdomain, response.content)
            )