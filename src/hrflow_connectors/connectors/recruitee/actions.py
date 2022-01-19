from typing import Iterator, Dict, Any, Optional, List
from pydantic import Field
import requests

from ...core.auth import AuthorizationAuth
from ...core.action import PullJobsBaseAction, PushProfileBaseAction
from ...utils.hrflow import generate_workflow_response
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags

logger = get_logger()


class PullJobsAction(PullJobsBaseAction):

    subdomain: str = Field(
        ..., description="the subdomain of your company's careers site."
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        pull all jobs from a recruitee subdomain endpoint

        Raises:
            ConnectionError: if the request failed, you may want to check your subdomain
        Returns:
            Iterator[Dict[str, Any]]: a list of jobs dictionaries
        """
        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = f"https://{self.subdomain}.recruitee.com/api/offers"
        pull_jobs_request.auth = self.auth
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            logger.error(
                f"Failed to get jobs for company: {self.subdomain}, Check that the subdomain is a valid one"
            )
            error_message = "Unable to pull the data ! Reason: `{}`, `{}`"
            raise ConnectionError(
                error_message.format(response.status_code, response.content)
            )
        response_dict = response.json()
        job_list = response_dict["offers"]
        return job_list

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a job into the hrflow job object format
        Args:
            data (Dict[str, Any]): a job object pulled from a recruitee company subdomain
        Returns:
            Dict[str, Any]: a job into the hrflow job object format
        """

        job = dict()
        # basic information
        job["name"] = data.get("title")
        job["summary"] = data.get("slug")
        job["url"] = data.get("careers_url")
        job["reference"] = str(data.get("id"))
        # location
        location = data.get("location")
        city = data.get("city")
        country = data.get("country")
        country_code = data.get("country_code")
        geojson = dict(city=city, country=country, country_code=country_code)
        job["location"] = dict(text=location, geojson=geojson, lat=None, lng=None)
        # sections
        description = remove_html_tags(data.get("description"))
        requirements = remove_html_tags(data.get("requirements"))
        job["sections"] = [
            dict(
                name="recruitee_description",
                title="recruitee_description",
                description=description,
            ),
            dict(
                name="recruitee_requirements",
                title="recruitee_requirements",
                description=requirements,
            ),
        ]
        job["created_at"] = data.get("created_at")

        # tags
        remote = str(data.get("remote"))
        category_code = str(data.get("category_code"))
        options_cv = str(data.get("options_cv"))
        min_hours = str(data.get("min_hours"))
        max_hours = str(data.get("max_hours"))
        options_cover_letter = str(data.get("options_cover_letter"))
        experience_code = str(data.get("experience_code"))
        company_name = data.get("company_name")
        department = str(data.get("department"))
        employment_type = str(data.get("employment_type_code"))
        education_code = str(str(data.get("education_code")))
        apply_url = data.get("careers_apply_url")
        job["tags"] = [
            dict(name="recruitee_remote", value=remote),
            dict(name="recruitee_category_code", value=category_code),
            dict(name="recruitee_options_cv", value=options_cv),
            dict(name="recruitee_options_cover_letter", value=options_cover_letter),
            dict(name="recruitee_min_hours", value=min_hours),
            dict(name="recruitee_max_hours", value=max_hours),
            dict(name="recruitee_experience_code", value=experience_code),
            dict(name="recruitee_employment_type", value=employment_type),
            dict(name="recruitee_education_code", value=education_code),
            dict(name="recruitee_company_name", value=company_name),
            dict(name="recruitee_department", value=department),
            dict(name="recruitee_apply_url", value=apply_url),
        ]

        return job


class PushProfileAction(PushProfileBaseAction):

    auth: AuthorizationAuth
    company_id: str = Field(
        ..., description="Company ID. A company subdomain can also be used."
    )
    offer_id: Optional[List[int]] = Field(
        None,
        description="Offers to which the candidate will be assigned with default stage. You can also pass one ID as offer_id",
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
        # Recruitee profile format is in the following format dict(candidate = profile, offers = [offer_id1,...])
        output_data = dict(candidate=profile)
        if self.offer_id is not None:
            output_data["offers"] = self.offer_id

        return output_data

    def push(self, data):
        profile = next(data)

        # Prepare request
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "POST"
        push_profile_request.url = (
            f"https://api.recruitee.com/c/{self.company_id}/candidates"
        )
        push_profile_request.auth = self.auth
        push_profile_request.json = profile
        prepared_request = push_profile_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            raise RuntimeError(
                f"Push profile to Recruitee failed :`{response.status_code}` `{response.content}`"
            )
