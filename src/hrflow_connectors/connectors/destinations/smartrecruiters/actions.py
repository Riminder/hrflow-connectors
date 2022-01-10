from pydantic import Field
from typing import Dict, Any
import requests

from ....core.action import PushProfileAction
from ....core.auth import XSmartTokenAuth
from ....utils.hrflow import generate_workflow_response


class SmartRecruitersPushProfileAction(PushProfileAction):
    auth: XSmartTokenAuth
    job_id: str = Field(
        ...,
        description="Id of a Job to which you want to assign a candidate when it’s created. A profile is sent to this URL `https://api.smartrecruiters.com/jobs/{job_id}/candidates` ",
    )

    def format(self, profile: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format a profile hrflow object to a smartrecruiters profile object

        Args:
            profile (Dict[str, Any]): [profile object in the hrflow profile format]

        Returns:
            Dict[str, Any]: [profile in the SmartRecruiters candidate application format]
        """

        value_or_undefined = lambda s: s or "Undefined"

        def format_project(project):
            formated_project_dict = dict()
            # current
            formated_project_dict["current"] = False
            # start date
            start_datetime_str = project.get("date_start")
            if start_datetime_str is None:
                # datetime is either in format YYYY or YYYY-MM...
                # if there is none we force this value to avoid conflict with smartrecruiters profile object
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
            formated_project_dict["location"] = value_or_undefined(
                project["location"]["text"]
            )
            formated_project_dict["description"] = project["description"]

            return formated_project_dict

        def format_educations(educations):
            formated_education_list = []

            for education_entity in educations:
                formated_education = format_project(education_entity)
                if education_entity.get("school") is None:
                    formated_education["instituion"] = "Undefined"
                else:
                    formated_education["institution"] = education_entity.get("school")

                if education_entity.get("title") is None:
                    formated_education["degree"] = "Undefined"
                else:
                    formated_education["degree"] = education_entity.get("title")
                formated_education["major"] = "Undefined"

                formated_education_list.append(formated_education)

            return formated_education_list

        def format_experiences(experiences):
            formated_experience_list = []

            for exp in experiences:
                formated_exp = format_project(exp)
                if exp["title"] is None:
                    formated_exp["title"] = "Undefined"
                else:
                    formated_exp["title"] = exp["title"]
                if exp["company"] is None:
                    formated_exp["company"] = "Undefined"
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
        ]:  # check if fields is not an undefined list
            smart_candidate["location"] = dict(
                city=value_or_undefined(info["location"]["fields"].get("city", {})),
                country=value_or_undefined(
                    info["location"]["fields"].get("country", {})
                ),
                region=value_or_undefined(info["location"]["fields"].get("state", {})),
                lat=info["location"]["lat"]
                if info["location"]["lat"] is not None
                else 0,
                lng=info["location"]["lng"]
                if info["location"]["lng"] is not None
                else 0,
            )
        else:
            smart_candidate["location"] = dict(
                country="Undefined",
                region="Undefined",
                City="Undefined",
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
        """
        Push profile

        Args:
            data (Dict[str, Any]): Profile
        """
        profile = next(data)

        # Prepare request
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "POST"
        push_profile_request.url = (
            f"https://api.smartrecruiters.com/jobs/{self.job_id}/candidates"
        )
        push_profile_request.auth = self.auth
        push_profile_request.json = profile
        prepared_request = push_profile_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            raise RuntimeError(
                f"Push profile to SmartRecruiters failed : `{response.content}`"
            )
