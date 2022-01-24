from typing import Iterator, Dict, Any, Union, List
from pydantic import Field
import requests

from ...core.error import PullError, PushError
from ...core.action import PullJobsBaseAction, PushProfileBaseAction
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.hrflow import generate_workflow_response
from ...core.auth import AuthorizationAuth

logger = get_logger()


class PullJobsAction(PullJobsBaseAction):
    auth: AuthorizationAuth

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull all jobs from a Teamtailor job board
        Returns:
            Iterator[Dict[str, Any]]: list of all jobs with their content if available
        """
        # Prepare request
        session = requests.Session()
        headers = {'X-Api-Version': '20210218'}
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = "https://api.teamtailor.com/v1/jobs"
        pull_jobs_request.auth = self.auth
        pull_jobs_request.headers = headers

        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            raise PullError(
                response,
                message="Failed to get jobs from Teamtailor.",
            )

        response_dict = response.json()

        job_list = response_dict["data"]
        return job_list

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format a Teamtailor job object into a Hrflow job object

        Args:
            data (Dict[str, Any]): Teamtailor job object from the list of jobs pulled

        Returns:
            Dict[str, Any]: a job in the HrFlow job object form
        """
        job = dict()
        attribute = data.get("attributes")
        job["name"] = attribute.get("title")
        job["reference"] = data.get("id")
        job["summary"] = attribute.get("pitch")
        job["created_at"] = attribute.get("created-at")
        job["updated_at"] = attribute.get("updated-at")
        job["url"] = data.get("links").get("careersite-job-url")

        # get location request
        job["location"] = dict(text=None, lat=None, lng=None)

        def get_location() -> None:
            """
             Get_location sends a request to get the job location from its API endpoint
            """
            session = requests.Session()
            pull_job_location_request = requests.Request()
            pull_job_location_request.method = "GET"
            id = job['reference']
            pull_job_location_request.url = (
                f'https://api.teamtailor.com/v1/jobs/{id}/location'
            )

            pull_job_location_request.headers = {'X-Api-Version': '20210218'}
            pull_job_location_request.auth = self.auth
            prepared_request = pull_job_location_request.prepare()
            response = session.send(prepared_request)
            if not response.ok:
                raise PullError(
                    response,
                    message="Failed to get job location from Teamtailor.",
                )
            location = response.json().get("data")
            if location is not None:
                location_attribute = location.get("attributes")
                text = location_attribute["address"]
                city = location_attribute["city"]
                country = location_attribute["country"]
                headquarters = location_attribute["headquarters"]
                lat = location_attribute["lat"]
                lng = location_attribute["long"]
                zip = location_attribute["zip"]
                name = location_attribute["name"]
                geojson = dict(
                    city=city,
                    country=country,
                    headquarters=headquarters,
                    zip=zip,
                    name=name,
                )
                job["location"] = dict(text=text, geojson=geojson, lat=lat, lng=lng)

        get_location()

        # sections
        description = remove_html_tags(attribute.get("body"))
        job["sections"] = [
            dict(
                name="teamtailor_description",
                title="teamtailor_description",
                description=description,
            )
        ]
        # tags
        job["tags"] = []

        def create_tag(field_name: str) -> None:
            tag_name = "teamtailor_{}".format(field_name)
            tag_value = attribute.get(field_name)

            if tag_value is not None:
                tag = dict(name=tag_name, value=tag_value)
                job["tags"].append(tag)

        create_tag("start-date")
        create_tag("end-date")
        create_tag("status")
        create_tag("employment-type")
        create_tag("employment-level")
        create_tag("remote-status")
        create_tag("salary-time-unit")
        create_tag("min-salary")
        create_tag("max-salary")
        create_tag("currency")
        create_tag("internal")

        return job
