from typing import Iterator, Dict, Any
from pydantic import Field
import requests

from ...core.error import PullError
from ...core.action import PullJobsBaseAction
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.schemas import HrflowJob
from .schemas import WorkableJobModel

logger = get_logger()


class PullJobsAction(PullJobsBaseAction):

    subdomain: str = Field(
        ...,
        description="subdomain of a company endpoint in `https://www.workable.com/api/accounts/{subdomain}` for example subdomain=`eurostar` for eurostar company",
    )

    def pull(self) -> Iterator[WorkableJobModel]:
        """
        pull all jobs from a workable public endpoint jobs stream
        Returns:
            Iterator[WorkableJobModel]: a list of workable job models
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
            raise PullError(
                response,
                message="Failed to get jobs. Check that the subdomain is a valid one",
                subdomain=self.subdomain,
            )

        response_dict = response.json()

        job_list = response_dict["jobs"]
        job_obj_iter = map(WorkableJobModel.parse_obj,job_list)
        return job_obj_iter

    def format(self, data: WorkableJobModel) -> HrflowJob:
        """
        format a job into the hrflow job object format
        Args:
            data (WorkableJobModel): a job object pulled from workable subdomain
        Returns:
            HrflowJob: a job into the hrflow job object format
        """
        job = dict()
        data = data.dict()
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
        job_obj = HrflowJob.parse_obj(job)

        return job_obj
