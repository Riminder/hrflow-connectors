from typing import Iterator, Dict, Any
from pydantic import Field
import requests
from ...core import action as core
from ...core.auth import XAPIKeyAuth
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.datetime_converter import seconds_to_isoformat

logger = get_logger()


class PullJobsAction(core.PullJobsAction):
    auth: XAPIKeyAuth
    page: int = Field(0, description="Page number. Start at '0'")
    page_size: int = Field(
        100, description="Page size. Max size of the list returned. Max value : 100"
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull jobs from a Taleez jobs owner endpoint 

        Returns list of all jobs that have been pulled
        """

        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = f"https://api.taleez.com/0/jobs?page={self.page}&pageSize={self.page_size}&withDetails=true"
        pull_jobs_request.auth = self.auth
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            logger.error(f"Failed to get jobs from this endpoint")
            error_message = "Unable to pull the data ! Reason : `{}` `{}`"
            raise ConnectionError(
                error_message.format(response.status_code, response.content)
            )

        response_dict = response.json()
        logger.info(f"Total found: {response_dict.get('listSize')}")
        job_list = response_dict.get("list")

        return job_list

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a job into the hrflow job object format

        Args:
            data (Dict[str, Any]): a taleez job object form

        Returns:
            Dict[str, Any]: a hrflow job object form
        """

        job = dict()

        # Job basic information
        job["name"] = data.get("label")
        job["reference"] = str(data.get("id"))
        job["url"] = data.get("url")
        job["summary"] = None

        # Job Location
        city = data.get("city")
        country = data.get("country")
        postalcode = data.get("postalCode")
        lat = data.get("lat")
        lng = data.get("lng")
        geojson = dict(country=country, postalcode=postalcode)
        job["location"] = dict(text=city, lat=lat, lng=lng, geojson=geojson)

        # Job Sections
        job["sections"] = []

        def create_section(field_name: str):
            section_name = "taleez_{}".format(field_name)
            section_tile = section_name
            description = remove_html_tags(data.get(field_name))
            if description is not None:
                section = dict(
                    name=section_name, title=section_tile, description=description
                )
                job["sections"].append(section)

        create_section("jobDescription")
        create_section("profileDescription")
        create_section("companyDescription")

        # Job Tags
        job["tags"] = []

        def create_tag(field_name: str):
            tag_name = "taleez_{}".format(field_name)
            tag_value = data.get(field_name)
            if tag_value is not None:
                tag = dict(name=tag_name, value=tag_value)
                job["tags"].append(tag)

        create_tag("contract")
        create_tag("profile")
        create_tag("contractLength")
        create_tag("fullTime")
        create_tag("workHours")
        create_tag("qualification")
        create_tag("remote")
        create_tag("recruiterId")
        create_tag("companyLabel")
        create_tag("urlApplying")
        create_tag("currentStatus")

        # datetime fields
        job["created_at"] = seconds_to_isoformat(data.get("dateCreation"))
        job["updated_at"] = seconds_to_isoformat(data.get("dateLastPublish"))

        return job
    