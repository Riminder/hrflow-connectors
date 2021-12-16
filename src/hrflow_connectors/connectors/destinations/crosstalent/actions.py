from ....core.auth import OAuth2PasswordCredentialsBody
from ....core.action import SourceDestinationAction
from ....core.http import HTTPStream
from ....utils.hrflow import generate_workflow_response

from pydantic import Field
from typing import Dict, Any


class PushProfile(SourceDestinationAction, HTTPStream):
    payload: Dict[str, Any] = dict()
    auth: OAuth2PasswordCredentialsBody
    subdomain: str = Field(
        ...,
        description="Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.salesforce.com/ABC`",
    )

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"

    @property
    def base_url(self):
        return "https://{}.salesforce.com/services/apexrest/crta/HrFlowCreateProfile".format(
            self.subdomain
        )

    @property
    def http_method(self):
        return "POST"

    def push(self, data):
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to Crosstalent failed : `{}`".format(response.content)
            )

    def execute(self):
        super().execute()
        return generate_workflow_response(
            status_code=201, message="Profile successfully pushed"
        )
