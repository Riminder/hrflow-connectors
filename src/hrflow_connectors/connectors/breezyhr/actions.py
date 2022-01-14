from typing import Iterator, Dict, Any, Optional
from pydantic import Field
import requests
from ...core import action as core
from ...core.auth import OAuth2EmailPasswordBody
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.hrflow import generate_workflow_response

logger = get_logger()


class PullJobsAction(core.PullJobsAction):
    auth: OAuth2EmailPasswordBody
    company_id: int= Field(..., description="ID of company to pull jobs from in Breezy HR database")

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull jobs from a Taleez jobs owner endpoint
        Returns list of all jobs that have been pulled
        """

        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = f"https://api.breezy.hr/v3/company/{self.company_id}/positions?"
        pull_jobs_request.auth = self.auth
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            logger.error(f"Failed to get jobs from this endpoint")
            error_message = "Unable to pull the data ! Reason : `{}` `{}`"
            raise RuntimeError(
                error_message.format(response.status_code, response.content)
            )

        return response.json()

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:

        job = dict()

        # Basic information
        job["name"] = data.get("name")
        job["reference"] = data.get("_id")
        job["summary"] = None

        # Location
        location = data.get("location")
        country = location.get("country")
        country_name = country.get("name")
        city = location.get("city")
        address = location.get("name")
        gejson = dict(country=country_name, city=city)
        
        job["location"] = dict(text=address, lat=None, lng=None)
        

