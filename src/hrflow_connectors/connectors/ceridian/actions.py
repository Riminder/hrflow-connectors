from typing import Iterator, Dict, Any
from pydantic import Field
import requests

from ...core.action import PullJobsBaseAction


class PullJobsAction(PullJobsBaseAction):

    subdomain: str = Field(..., description="subdomain just before `dayforcehcm.com`")
    client_name_space: str = Field(
        ...,
        description="Uniquely identifies the client's Dayforce instance. Is needed to login",
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        pull all jobs from a ceridian dayforce job feed space
        Raises:
            ConnectionError: if the request failed, you may want to check your subdomain or client name space
        Returns:
            Iterator[Dict[str, Any]]: a list of jobs dictionaries
        """
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = f"https://{self.subdomain}.dayforcehcm.com/Api/{self.client_name_space}/V1/JobFeeds"
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:

            error_message = "Unable to pull the data ! Reason : `{}`, `{}`"
            raise ConnectionError(
                error_message.format(response.status_code, response.content)
            )
        return response.json()

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a job into the hrflow job object format
        Args:
            data (Dict[str, Any]): a job object pulled from a ceridian dayforce job fed space
        Returns:
            Dict[str, Any]: a job into the hrflow job object format
        """
        job = dict()
        # basic information
        job["name"] = data.get("Title")
        job["summary"] = None
        job["reference"] = str(data.get("ReferenceNumber")) + str(
            data.get("ParentRequisitionCode")
        )
        job["url"] = data.get("JobDetailsUrl")
        # location
        location = data.get("City")
        state = data.get("State")
        country = data.get("Country")
        postal_code = data.get("PostalCode")
        geojson = dict(state=state, country=country, postal_code=postal_code)
        job["location"] = dict(text=location, lat=None, lng=None, geojson=geojson)
        # sections
        description = data.get("Description")
        job["sections"] = [
            dict(
                name="dayforce_description",
                title="dayforce_description",
                description=description,
            )
        ]
        job["created_at"] = data.get("DatePosted")
        job["updated_at"] = data.get("LastUpdated")

        # tags
        apply_url = str(data.get("ApplyUrl"))
        client_site_name = str(data.get("ClientSiteName"))
        client_site_ref_code = str(data.get("ClientSiteXRefCode"))
        company_name = str(data.get("CompanyName"))
        remote = str(data.get("IsVirtualLocation"))
        job["tags"] = [
            dict(name="dayforce_apply_url", value=apply_url),
            dict(name="dayforce_client-site-name", value=client_site_name),
            dict(name="dayforce_client-site-ref-code", value=client_site_ref_code),
            dict(name="dayforce_company_name", value=company_name),
            dict(name="dayforce_remote", value=remote),
        ]

        return job
