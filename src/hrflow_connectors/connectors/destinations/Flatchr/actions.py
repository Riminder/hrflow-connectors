from ....core.auth import APIKeyAuth
from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....utils.hrflow import generate_workflow_response

from pydantic import Field
from typing import Dict, Any, Iterator
import base64
import requests


class PushProfile(ProfileDestinationAction, HTTPStream):
    payload: Dict[str, Any] = dict()
    auth: APIKeyAuth
    subdomain: str = Field(
        ...,
        description="Subdomain Flatchr just before `flatchr.io`. For example subdomain=`my_subdomain.my` in "
                    "`http://my_subdomain.my.flatchr.io/ABC`",
    )
    vacancy: str = Field(
        ...,
        description="The pool in which candidates will be placed. Findable in the URL",
    )

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"
        self.headers['Accept'] = "*/*"

    @property
    def base_url(self):
        return "https://{}.flatchr.io/vacancy/candidate/json".format(
            self.subdomain
        )

    @property
    def http_method(self):
        return "POST"

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull data
        """
        response = self.hrflow_client.profile.indexing.get(
            source_key=self.profile.source.key, key=self.profile.key
        )
        if response["code"] >= 400:
            raise RuntimeError(
                "Indexing profile get failed : `{}`".format(response["message"])
            )

        profile = response["data"]
        formated_data = map(self.format, profile)
        return list(formated_data)

    def push(self, data):
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to Flatchr failed : `{}`".format(response.content)
            )

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:

        def get_candidate_attachments(hrflow_profile):
            attachments_list = hrflow_profile.get("attachments")
            # We try to find 'resume' (which is 'original' after traitments).
            # If we don't find it, we search 'original'.
            fall_backs = ["resume", "original"]

            for file_name in fall_backs:
                for attachment in attachments_list:
                    if attachment["file_name"] == file_name:
                        # Get the base64's resume from its AWS url.
                        url = attachment["public_url"]
                        response = requests.get(url)
                        b64 = base64.b64encode(response.content)
                        return b64.decode()
            return None

        info = data.get("info")
        email = info.get("email") if info else None
        if email == None:
            raise Exception(f"No email for hrflow_profile {data.get('reference')} but one is mandatory")

        profile = {
            "vacancy": self.vacancy,
            "firstname": info.get("first_name") if info.get("first_name") is not None else "N/A",
            "lastname": info.get("last_name") if info.get("last_name") is not None else "N/A",
            "type": "document",
            "email": email,
            "resume": {
                "fileName": "resume.pdf",
                "contentType": "application/octet-stream",
                "data": get_candidate_attachments(data)
            }
        }
        return profile

    def execute(self):
        super().execute()
        return generate_workflow_response(
            status_code=201, message="Profile successfully pushed"
        )
