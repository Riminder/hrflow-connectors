from pydantic import Field
import xml.etree.ElementTree
from typing import Iterator
import requests

from ...core.action import PullJobsBaseAction


class PullJobsAction(PullJobsBaseAction):
    xml_stream_url: str = Field(..., description="URL to XML Stream")
    job_list_xpath: str = Field(
        ..., description="XPath pointing to the job list in the XML stream"
    )

    def pull(self) -> Iterator[xml.etree.ElementTree.Element]:
        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = self.xml_stream_url
        pull_jobs_request.auth = self.auth
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

        xml_stream = response.content
        root_element = xml.etree.ElementTree.fromstring(xml_stream)
        job_list_element = root_element.find(self.job_list_xpath)
        job_list = list(job_list_element)
        return job_list
