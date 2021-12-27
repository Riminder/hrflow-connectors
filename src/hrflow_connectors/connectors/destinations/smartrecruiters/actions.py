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

    def format(self, profile: Dict[str, Any]) -> Dict[str, Any]:
        """
        format [converts a profile hrflow object into a smartrecruiters profile object]

        Args:
            profile (Dict[str, Any]): [profile object in the hrflow profile format]

        Returns:
            Dict[str, Any]: [profile in the SmartRecruiters candidate application format]
        """

        xstr = lambda s: s or " "

        def format_project(project):

            return {
                "current": False,
                "startDate": project["date_start"].split("T")[0]
                if project["date_start"]
                else "NaN",
                "endDate": project["date_end"].split("T")[0]
                if project["date_end"]
                else "Nan",
                "location": xstr(project["location"]["text"]),
                "description": project["description"],
            }

        def format_educations(educations):
            _educations = []

            for edu in educations:
                _edu = format_project(edu)
                _edu.update(
                    {
                        "institution": edu.get("school")
                        if edu.get("school")
                        else "NaN",
                        "degree": edu.get("title") if edu.get("title") else "NaN",
                        "major": "NaN",
                    }
                )

                _educations.append(_edu)

            return _educations

        def format_experiences(experiences):
            _experiences = []

            for exp in experiences:
                _exp = format_project(exp)
                _exp.update(
                    {
                        "title": exp["title"] if exp["title"] else "NaN",
                        "company": exp["company"] if exp["company"] else "NaN",
                    }
                )

                _experiences.append(_exp)

            return _experiences

        info = profile["info"]
        smart_candidate = dict()
        smart_candidate["firstName"] = info["first_name"]
        smart_candidate["lastName"] = info["last_name"]
        smart_candidate["email"] = info["email"]
        smart_candidate["phoneNumber"] = info["phone"]
        smart_candidate["location"] = dict(
            country=xstr(info["location"]["fields"]["country"]),
            countryCode="No",
            region=xstr(info["location"]["fields"]["state"]),
            regionCode="NaN",
            city=xstr(info["location"]["fields"]["city"]),
            lat=info["location"]["lat"] if info["location"]["lat"] else "NaN",
            lng=info["location"]["lng"] if info["location"]["lng"] else "NaN",
        )
        smart_candidate["web"] = dict(info["urls"])
        smart_candidate["tags"] = []
        smart_candidate["education"] = format_educations(profile.get("educations"))
        smart_candidate["experience"] = format_experiences(profile.get("experiences"))
        smart_candidate["attachments"] = profile.get("attachments")
        smart_candidate["consent"] = True

        return smart_candidate

    def push(self, data: Dict[str, Any]):
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to SmartRecruiters failed : `{}`".format(response.content)
            )

    def execute(self):
        super().execute()
        return generate_workflow_response(
            status_code=201, message="Profile successfully pushed"
        )