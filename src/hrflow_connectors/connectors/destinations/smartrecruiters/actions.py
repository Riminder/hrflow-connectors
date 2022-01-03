from pydantic import Field
from typing import Dict, Any
from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....core.auth import XSmartTokenAuth
from ....utils.hrflow import generate_workflow_response


class SmartProfile(ProfileDestinationAction, HTTPStream):
    payload: Dict[str, Any] = dict()
    auth: XSmartTokenAuth
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

        value_or_empty = lambda s: s or " "

        def format_project(project):
            formated_project_dict = dict()
            # current
            formated_project_dict["current"] = False
            # start date
            start_datetime_str = project.get("date_start")
            if start_datetime_str is None:
                start_date = "XXXX"
            else:
                start_date = start_datetime_str.split("T")[0]

            formated_project_dict["startDate"] = start_date

            end_datetime_str = project.get("date_end")
            if end_datetime_str is None:
                end_date = "XXXX"
            else:
                end_date = end_datetime_str.split("T")[0]

            formated_project_dict["endDate"] = end_date
            formated_project_dict["location"] = value_or_empty(
                project["location"]["text"]
            )
            formated_project_dict["description"] = project["description"]

            return formated_project_dict

        def format_educations(educations):
            formated_education_list = []

            for education_entity in educations:
                formated_education = format_project(education_entity)
                if education_entity.get("school") is None:
                    formated_education["instituion"] = "NaN"
                else:
                    formated_education["institution"] = education_entity.get("school")

                if education_entity.get("title") is None:
                    formated_education["degree"] = "Nan"
                else:
                    formated_education["degree"] = education_entity.get("title")
                formated_education["major"] = "NaN"

                formated_education_list.append(formated_education)

            return formated_education_list

        def format_experiences(experiences):
            formated_experience_list = []

            for exp in experiences:
                formated_exp = format_project(exp)
                if exp["title"] is None:
                    formated_exp["title"] = "NaN"
                else:
                    formated_exp["title"] = exp["title"]
                if exp["company"] is None:
                    formated_exp["company"] = "NaN"
                else:
                    formated_exp["company"] = exp["company"]

                formated_experience_list.append(formated_exp)

            return formated_experience_list

        info = profile["info"]
        smart_candidate = dict()
        smart_candidate["firstName"] = info["first_name"]
        smart_candidate["lastName"] = info["last_name"]
        smart_candidate["email"] = info["email"]
        smart_candidate["phoneNumber"] = info["phone"]

        if info["location"]["fields"] not in [
            [],
            None,
        ]:  # check if fields is not an empty list
            smart_candidate["location"] = dict(
                country=value_or_empty(info["location"]["fields"].get("country", {})),
                countryCode="No",
                region=value_or_empty(info["location"]["fields"].get("state", {})),
                regionCode="NaN",
                city=value_or_empty(info["location"]["fields"].get("city", {})),
                lat=info["location"]["lat"] if info["location"]["lat"] else 0,
                lng=info["location"]["lng"] if info["location"]["lng"] else 0,
            )
        else:
            smart_candidate["location"] = dict(
                country="Not shown",
                countryCode="NO",
                regionCode="Not shown",
                City="Not shown",
                lat=0,
                lng=0,
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
