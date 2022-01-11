from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from pydantic import Field
from typing import Dict, Any, Optional, Union, List
from ....utils.logger import get_logger

logger = get_logger()


class PushProfile(ProfileDestinationAction, HTTPStream):

    auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]
    payload: Dict[str, Any] = dict()
    job_id: List[int] = Field(
        ...,
        description="The internal ID of the job to which this candidate should be added can be a list of jobs",
    )
    on_behalf_of: str

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"
        self.headers["on-behalf-of"] = self.on_behalf_of

    @property
    def base_url(self):
        return "https://harvest.greenhouse.io/v1/candidates"

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
        profile["applications"] = []
        for id in self.job_id:
            profile["applications"].append(dict(job_id=id))

        if self.job_id is not None:
            profile["job_id"] = self.job_id

        profile["first_name"] = data.get("info").get("first_name")
        profile["last_name"] = data.get("info").get("last_name")
        profile["external_id"] = data.get("reference")

        if data.get("attachments") not in [[], None]:
            profile["resume"] = data.get("attachments")[0]["public_url"]

        phone_number = data.get("info").get("phone")
        profile["phone_numbers"] = [dict(value=phone_number, type="mobile")]

        email = data.get("info").get("email")
        profile["email_addresses"] = [dict(value=email, type="personal")]

        address = data.get("info").get("location").get("text")
        profile["addresses"] = [dict(value=address, type="home")]

        profile["notes"] = data.get("text")

        def get_social_media_urls():
            urls = data["info"]["urls"]
            website_list = []
            for url in urls:
                if url["url"] not in ["", None, []]:
                    website_list.append(dict(value=url["url"]))
            return website_list

        if get_social_media_urls() not in [[], None]:
            profile["social_media_addresses"] = get_social_media_urls()

        if data["experiences"] not in [[], None]:
            last_experience = data["experiences"][0]
            profile["company"] = last_experience["company"]
            profile["title"] = last_experience["title"]
            profile["employments"] = []
            for experience in data["experiences"]:
                if (
                    experience["title"]
                    and experience["company"]
                    and experience["date_start"]
                ) not in ["", None]:
                    profile["employments"].append(
                        dict(
                            company_name=experience["company"],
                            title=experience["title"],
                            start_date=experience["date_start"],
                            end_date=experience["date_end"],
                        )
                    )

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
        logger.debug(f"{response.status_code},{response.content}")
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to Greenhouse failed : {}, `{}`".format(
                    response.status_code, response.content
                )
            )
