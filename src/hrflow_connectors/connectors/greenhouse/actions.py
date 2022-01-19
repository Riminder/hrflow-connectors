from typing import Iterator, Dict, Any, Union, List
from pydantic import Field
import html
import requests

from ...core.action import PullJobsBaseAction, PushProfileBaseAction
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.hrflow import generate_workflow_response
from ...core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth

logger = get_logger()


class PullJobsAction(PullJobsBaseAction):
    board_token: str = Field(
        ...,
        description="Job Board URL token, which is usually the company `name` -for example `lyft`- when it has job listings on greenhouse, mandatory to access job boards on `greenhouse.io`: `https://boards-api.greenhouse.io/v1/boards/{board_token}/jobs`, getting jobs doesn't require an API Key",
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull all jobs from a greenhouse job board

        Returns:
            Iterator[Dict[str, Any]]: list of all jobs with their content if available
        """
        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = f"https://boards-api.greenhouse.io/v1/boards/{self.board_token}/jobs/?content=true"
        pull_jobs_request.auth = self.auth
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            logger.error(
                f"Failed to get jobs from board: `{self.board_token}`. Check that your board token is valid."
            )
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

        response_dict = response.json()
        total_info = response_dict["meta"]["total"]
        logger.info(f"Total jobs found : {total_info}")

        job_list = response_dict["jobs"]
        return job_list

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format each job pulled from greenhouse job board into a HrFlow job object

        Returns:
            Dict[str, Any]: job in the HrFlow job object format
        """

        job = dict()
        # name
        job["name"] = data.get("title")
        # summary
        job["summary"] = None
        # reference
        job["reference"] = str(data.get("id"))
        # url
        job["url"] = data.get("absolute_url")
        # location
        location = data.get("location").get("name")
        job["location"] = dict(text=location, lat=None, lng=None)
        # sections
        description_content = data.get("content")
        # convert the escaped description content into html format
        description_html = html.unescape(description_content)
        # remove html tags to get clean text
        text = remove_html_tags(description_html)

        job["sections"] = [
            dict(
                name="greenhouse_description",
                title="greenhouse_description",
                description=text,
            )
        ]
        # metadata
        job["metadatas"] = data.get("metadata")
        # tags
        department = data.get("departments")
        if department not in [None, []]:
            department_name = department[0].get("name")
            department_id = str(department[0].get("id"))
        else:
            department_name = "Undefined"
            department_id = "Undefined"

        office = data.get("offices")
        if office not in [None, []]:
            office_name = office[0].get("name")
            office_id = str(office[0].get("id"))
        else:
            office_name = "Undefined"
            office_id = "Undefined"

        education = data.get("education")
        employment = data.get("employment")

        job["tags"] = [
            dict(name="greenhouse_department-name", value=department_name),
            dict(name="greenhouse_department-id", value=department_id),
            dict(name="greenhouse_office-location", value=office_name),
            dict(name="greenhouse_office-id", value=office_id),
            dict(name="greenhouse_education", value=education),
            dict(name="greenhouse_employment", value=employment),
        ]
        # updated_at
        job["updated_at"] = data.get("updated_at")

        return job


class PushProfileAction(PushProfileBaseAction):

    auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]
    job_id: List[int] = Field(
        ...,
        description="List of jobs internal ids to which the candidate should be added",
    )
    on_behalf_of: str = Field(
        ...,
        description="The ID of the user sending the profile, or the person he is sending the profile on behalf of",
    )

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

        profile = next(data)

        # Prepare request
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "POST"
        push_profile_request.url = "https://harvest.greenhouse.io/v1/candidates"
        push_profile_request.auth = self.auth
        push_profile_request.headers = {"on-behalf-of": self.on_behalf_of}
        push_profile_request.json = profile
        prepared_request = push_profile_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            raise RuntimeError(
                "Push profile to Greenhouse failed : {}, `{}`".format(
                    response.status_code, response.content
                )
            )
