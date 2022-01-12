from typing import Iterator, Dict, Any
from pydantic import Field
from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....utils.logger import get_logger

logger = get_logger()


class PullJobs(HTTPStream, BoardAction):
    subdomain: str = Field(..., description="subdomain just before `dayforcehcm.com`")
    client_name_space: str = Field(
        ...,
        description="Uniquely identifies the client's Dayforce instance. Is needed to login",
    )

    @property
    def base_url(self):
        return "https://{}.dayforcehcm.com/Api/{}/V1/JobFeeds".format(
            self.subdomain, self.client_name_space
        )

    @property
    def http_method(self):
        return "GET"

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        pull all jobs from a ceridian dayforce job feed space

        Raises:
            ConnectionError: if the request failed, you may want to check your subdomain or client name space

        Returns:
            Iterator[Dict[str, Any]]: a list of jobs dictionaries
        """

        response = self.send_request()
        if response.ok:
            job_dict_list = response.json()
            return job_dict_list
        else:
            logger.error(
                f"Failed to pull jobs, check that your subdomain: {self.subdomain}, and client name space: {self.client_name_space}"
            )
            error_message = "Unable to pull the data ! Reason : `{}`, `{}`"
            raise ConnectionError(
                error_message.format(response.status_code, response.content)
            )

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a job into the hrflow job object format
        Args:
            data (Dict[str, Any]): a job object pulled from a ceridian dayforce job fed space
        Returns:
            Dict[str, Any]: a job into the hrflow job object format
        """
        job = dict()
        #basic information
        job["name"] = data.get("Title")
        job["summary"] = None
        job["reference"] = str(data.get("ReferenceNumber")) + str(data.get("ParentRequisitionCode"))
        job["url"] = data.get("JobDetailsUrl")
        # location
        location = data.get("city")
        state = data.get("state")
        country = data.get("country")
        postal_code = data.get("PostalCode")
        geojson = dict(state=state, country=country, postal_code=postal_code)
        job["location"] = dict(text=location, lat=None, lng=None, geojson=geojson)
        #sections
        description = data.get("Description")
        job["sections"] = [
            dict(
                name="dayforce_description",
                title="dayforce_description",
                description=description,
            )
        ]
        job["created_at"] = data.get("DatePosted")
        job["updated_at"] = data.get("DatePosted")

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
