from ....core.auth import SmartToken
from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from pydantic import Field
from typing import Dict, Any
from ....utils.hrflow import generate_workflow_response


class SmartCandidate(ProfileDestinationAction, HTTPStream):
    payload: Dict[str, Any] = dict()
    auth: SmartToken
    job_uuid: str = Field(
        ...,
        description="You need the `UUID` of the job to push the candidate, in hrflow smart jobs boards it is obtained in the `tags` section, you can also contact smartrecruiters to obtain the `UUID`, see `https://help.smartrecruiters.com/?title=Marketplace_Partners%2F4.Career_site_builders_%26_sourcing_tools%2FMethods_of_pushing_candidates_to_Smartrecruiters' for more",
    )

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"

    @property
    def base_url(self):
        return "https://api.smartrecruiters.com/jobs/{}/candidates".format(
            self.job_uuid
        )

    @property
    def http_method(self):
        return "POST"

    def push(self, data):
        self.payload.clear()
        profile = next(data)
        self.payload.udpate(profile)
        response = self.send_request()
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to SmartRecruiters failed : `{}`".format(response.content)
            )

    def execute(self):
        super.execute()
        return generate_workflow_response(
            status_code=201, message="Profile successfully pushed"
        )
