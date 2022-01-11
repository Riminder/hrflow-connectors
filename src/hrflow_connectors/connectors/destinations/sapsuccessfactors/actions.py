from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from pydantic import Field
from typing import Dict, Any, Union
import dateutil.parser as dp
from ....utils.logger import get_logger

logger = get_logger()


class PushProfile(ProfileDestinationAction, HTTPStream):

    auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]
    payload: Dict[str, Any] = dict()
    api_server: str = Field(
        ...,
        description="the API server for your company from the list of API servers for SAP SuccessFactors data centers",
    )

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"
        self.headers["Accept"] = "application/json"

    @property
    def base_url(self):
        return "https://{}/odata/v2/Candidate".format(self.api_server)

    @property
    def http_method(self):
        return "POST"

    def format(self, profile: Dict[str, Any]) -> Dict[str, Any]:

        candidate = dict()
        info = profile.get("info")

        candidate["address"] = info.get("location").get("text")
        candidate["cellPhone"] = info.get("phone")
        fields = info.get("location").get("fields")
        if fields not in [None, []]:
            candidate["country"] = fields.get("country")[:-1]
            candidate["city"] = fields.get("city")
            candidate["zip"] = fields.get("postcode")

        candidate["primaryEmail"] = info.get("email")
        candidate["firstName"] = info.get("first_name")
        candidate["lastName"] = info.get("last_name")
        candidate["currentTitle"] = info.get("summary")

        def format_start_date(date):
            return "/Date({})/".format(int(dp.parse(date).timestamp()))

        def format_end_date(date):
            return "/Date({})/".format(int(dp.parse(date).timestamp() + 10))

        if profile.get("educations") is not None:

            def format_education(education):
                result = dict()

                if (
                    education.get("date_end") is not None
                    and education.get("date_start") is not None
                ):
                    result["endDate"] = format_end_date(education.get("date_end"))
                    result["startDate"] = format_start_date(education.get("date_start"))

                result["school"] = education.get("school")
                result["schoolAddress"] = education.get("location").get("text")
                if result["schoolAddress"] is None:
                    result["schoolAddress"] = "Undefined"
                return result

            candidate["education"] = dict(results=[])
            for education in profile.get("educations"):
                candidate["education"]["results"].append(format_education(education))

        if profile.get("experiences") is not None:

            def format_experience(experience):
                result = dict()
                result["employer"] = experience.get("company")
                result["employerAddress"] = experience.get("location").get("text")
                if result["employerAddress"] is None:
                    result["employerAddress"] = "Undefined"
                if (
                    experience.get("date_end") is not None
                    and experience.get("date_start") is not None
                ):
                    result["endDate"] = format_end_date(experience.get("date_end"))
                    result["startDate"] = format_start_date(
                        experience.get("date_start")
                    )
                return result

            candidate["outsideWorkExperience"] = dict(results=[])
            for experience in profile.get("experiences"):
                candidate["outsideWorkExperience"]["results"].append(
                    format_experience(experience)
                )

        return candidate

    def push(self, data: Dict[str, Any]):
        """
        Push profile
        Args:
            data (Dict[str, Any]): Profile
        """
        profile_already_exist = b'{\n"error" : {\n"code" : "COE_GENERAL_SERVER_FAILURE", "message" : {\n"lang" : "en-US", "value" : "[COE0019]Couldn\'t create candidate due to return value:Candidate already exists with the index 0"\n}\n}\n}'
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        logger.debug(f"{response.status_code}, {response.content}")
        if response.status_code >= 400:
            if response.content == profile_already_exist:
                logger.warning(f"profile already exists")
            else:
                raise RuntimeError(
                    "Push profile to sapsuccesfactors api-server: {} failed : `{}`".format(
                        self.api_server, response.content
                    )
                )
