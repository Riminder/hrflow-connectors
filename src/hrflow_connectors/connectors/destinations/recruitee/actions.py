from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....core.auth import AuthorizationAuth
from pydantic import Field
from typing import Dict, Any, Optional, List
from ....utils.logger import get_logger

logger = get_logger()


class PushProfile(ProfileDestinationAction, HTTPStream):

    auth: AuthorizationAuth
    payload: Dict[str, Any] = dict()
    company_id: str = Field(
        ..., description="Company ID. A company subdomain can also be used."
    )
    offer_id: Optional[List[int]] = Field(
        None,
        description="Offers to which the candidate will be assigned with default stage. You can also pass one ID as offer_id",
    )

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"

    @property
    def base_url(self):
        return "https://api.recruitee.com/c/{}/candidates".format(self.company_id)

    @property
    def http_method(self):
        return "POST"

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
                "Push profile to Recruitee failed : {}, `{}`".format(
                    response.status_code, response.content
                )
            )

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a HrFlow Profile object into a Recruitee profile Object
        returns Dict[str, Any]: a profile in the format of Recruitee profiles
        """
        profile = dict()
        info = data.get("info")
        profile["name"] = info.get("full_name")
        if data.get("attachments") not in [None, []]:
            profile["remote_cv_url"] = data.get("attachments")[0].get("public_url")
        profile["emails"] = [str(info.get("email"))]
        profile["phones"] = [str(info.get("phone"))]

        def urls():
            urls = info.get("urls")
            website_list = []
            for url in urls:
                if url["url"] not in ["", None, []]:
                    website_list.append(url["url"])
            return website_list

        if urls() not in ["", None, []]:
            profile["links"] = urls()
        output_data = dict(candidate = profile)
        if self.offer_id is not None:
            output_data["offers"] = self.offer_id

        return output_data
