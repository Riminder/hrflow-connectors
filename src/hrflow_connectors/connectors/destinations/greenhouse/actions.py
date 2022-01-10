from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from pydantic import Field
from typing import Dict, Any, Optional, Union, List


class PushProfile(ProfileDestinationAction, HTTPStream):

    auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]
    payload: Dict[str, Any] = dict()
    prospect: bool = Field(
        True,
        description="True if this candidate should be a prospect. The organization must be able to create prospects to set this field. (Default: true)",
    )
    job_id: Optional[int] = Field(
        None,
        description="Required only if prospect is false. The ID of the job to which this candidate or prospect should be added",
    )
    on_behalf_of: Optional[str]

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"
        if self.on_behalf_of is not None:
            self.headers["On-Behalf-Of"] = self.on_behalf_of

    @property
    def base_url(self):
        return "https://api.greenhouse.io/v1/partner/candidates"

    @property
    def http_method(self):
        return "POST"

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format a profile hrflow object to a greenhouse profile object
        Args:
            profile (Dict[str, Any]): profile object in the hrflow profile format
        Returns:
            Dict[str, Any]: profile in the greenhouse candidate  format
        """
        profile = dict()
        profile["prospect"] = self.prospect

        if self.job_id is not None:
            profile["job_id"] = self.job_id

        profile["first_name"] = data.get("info").get("first_name")
        profile["last_name"] = data.get("info").get("last_name")
        profile["external_id"] = data.get("reference")


        if data.get('attachments') not in [[], None]:
            profile["resume"] = data.get("attachments")[0]['public_url']

        phone_number = data.get("info").get("phone")
        profile["phone_numbers"] = [dict(phone_number=phone_number, type="mobile")]

        email = data.get("info").get("email")
        profile["emails"] = [dict(email=email, type="other")]

        address = data.get("info").get("location").get("text")
        profile["addresses"] = [dict(address=address, type="home")]

        profile["notes"] = data.get("text")

        def get_social_media_urls():
            urls = data["info"]["urls"]
            website_list = []
            for url in urls:
                if url not in ["", None,[]]:
                    website_list.append({"url": url})
            return website_list

        if get_social_media_urls() not in [[], None]:
            profile["social_media"] = get_social_media_urls()

        if data["experiences"] not in [[], None]:
            last_experience = data["experiences"][0]
            profile["company"] = last_experience["company"]
            profile["title"] = last_experience["title"]

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
                "Push profile to Greenhouse failed : {}, `{}`".format(response.status_code, response.content)
            )