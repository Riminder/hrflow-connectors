from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream

from pydantic import Field
from typing import Dict, Any, Optional


class PushProfile(ProfileDestinationAction, HTTPStream):
    payload : Dict[str, Any] = dict()
    prospect: bool = Field(
        True,
        description="True if this candidate should be a prospect. The organization must be able to create prospects to set this field. (Default: true)",
    )
    job_id: Optional[int] = Field(
        ...,
        description="Required only if prospect is false. The ID of the job to which this candidate or prospect should be added",
    )

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"

    @property
    def base_url(self):
        return "n"

    @property
    def http_method(self):
        return "POST"

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        profile = dict()
        profile["prospect"] = self.prospect
        if self.job_id is not None:
            profile["job_id"] = self.job_id
        profile["first_name"] = data.get("info").get("first_name")
        profile["last_name"] = data.get("info").get("last_name")
        profile["external_id"] = data.get("reference")

        def get_attachment():
            attachments_list = data.get("attachments")
            fall_backs = ["resume", "original"]
            for file_name in fall_backs:
                for attachment in attachments_list:
                    if attachment["file_name"] == file_name:
                        resume_url = attachment["public_url"]
            return resume_url

        profile["resume"] = get_attachment(data)
        phone_number = data.get("info").get("phone")
        profile["phone_numbers"] = [dict(phone_number=phone_number, type="mobile")]
        email = data.get("info").get("email")
        profile["emails"] = [dict(email=email, type="other")]
        address = profile.get("info").get("location").get("text")
        profile["addresses"] = [dict(address=address, type="home")]
        profile["notes"] = data.get("text")

        return profile