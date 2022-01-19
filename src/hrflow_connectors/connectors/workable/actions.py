from typing import Iterator, Dict, Any
from pydantic import Field
import requests

from ...core.action import PullJobsBaseAction
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags

logger = get_logger()


class PullJobsAction(PullJobsBaseAction):

    subdomain: str = Field(
        ...,
        description="subdomain of a company endpoint in `https://www.workable.com/api/accounts/{subdomain}` for example subdomain=`eurostar` for eurostar company",
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        pull all jobs from a workable public endpoint jobs stream
        Returns:
            Iterator[Dict[str, Any]]: a list of jobs dictionaries
        """

        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = (
            f"https://www.workable.com/api/accounts/{self.subdomain}?details=True"
        )
        prepared_request = pull_jobs_request.prepare()

        # Send Request
        response = session.send(prepared_request)

        if not response.ok:
            logger.error(
                f"Failed to get jobs from subdomain: {self.subdomain}. Check that the subdomain is a valid one"
            )
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

        response_dict = response.json()

        job_list = response_dict["jobs"]
        return job_list

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a job into the hrflow job object format
        Args:
            data (Dict[str, Any]): a job object pulled from workable subdomain
        Returns:
            Dict[str, Any]: a job into the hrflow job object format
        """
        job = dict()
        # name and reference
        job["name"] = data.get("title")
        job["reference"] = data.get("shortcode")
        # location
        country = data.get("country")
        state = data.get("state")
        city = data.get("city")
        geojson = dict(country=country, state=state)
        job["location"] = dict(
            text=city,
            lat=None,
            lng=None,
            geojson=geojson,
        )
        # url
        job["url"] = data.get("url")
        # sections
        description = remove_html_tags(data.get("description"))
        job["sections"] = [
            dict(
                name="workable_description",
                title="workable_description",
                description=description,
            )
        ]
        # creation_date
        job["created_at"] = data.get("created_at")
        # tags
        employment_type = data.get("employment_type")
        industry = data.get("industry")
        function = data.get("function")
        experience = data.get("experience")
        education = data.get("education")
        application_url = data.get("application_url")
        department = data.get("department")
        job["tags"] = [
            dict(name="workable_employment_type", value=employment_type),
            dict(name="workable_indsutry", value=industry),
            dict(name="workable_function", value=function),
            dict(name="workable_experience", value=experience),
            dict(name="workable_education", value=education),
            dict(name="workable_application_url", value=application_url),
            dict(name="workable_department", value=department),
        ]
        job["metadatas"] = []

        return job
