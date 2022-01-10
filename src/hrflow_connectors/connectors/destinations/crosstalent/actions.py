from pydantic import Field
from typing import Dict, Any
import requests

from ....core.auth import OAuth2PasswordCredentialsBody
from ....core.action import PushProfileAction
from ....utils.hrflow import generate_workflow_response


class CrosstalentPushProfileAction(PushProfileAction):
    auth: OAuth2PasswordCredentialsBody
    subdomain: str = Field(
        ...,
        description="Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.salesforce.com/ABC`",
    )

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
        profile = next(data)

        # Prepare request
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "POST"
        push_profile_request.url = f"https://{self.subdomain}.salesforce.com/services/apexrest/crta/HrFlowCreateProfile"
        push_profile_request.auth = self.auth
        push_profile_request.json = profile
        prepared_request = push_profile_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            raise RuntimeError(
                f"Push profile to Crosstalent failed : `{response.content}`"
            )
