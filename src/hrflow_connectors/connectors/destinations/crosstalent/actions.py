from ....core.auth import OAuth2PasswordCredentialsBody
from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....utils.hrflow import generate_workflow_response

from pydantic import Field
from typing import Dict, Any


class PushProfile(ProfileDestinationAction, HTTPStream):
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

    def format(self, data):
        firstname = data["info"].get("first_name")
        lastname = data["info"].get("last_name")
        email = data["info"].get("email")
        key = data["key"]

        # Case if lastname is not defined after CV parsing
        # This field is required for Crosstalent
        if lastname is None or lastname == "":
            data["info"]["last_name"] = "N/A"

        # Case if email is not defined after CV parsing
        # This field is required for Crosstalent
        if email is None or email == "":
            if firstname is not None and lastname is not None:
                email = f"{firstname}.{lastname}@vulcain.com"
            else:
                email = f"{key}@vulcain.com"
            data["info"]["email"] = email

        return data

    def push(self, data):
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to Crosstalent failed : `{}`".format(response.content)
            )
