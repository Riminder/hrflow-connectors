from ....core.action import BoardAction
from ....core.http import HTTPStream


from pydantic import Field
import xml.etree.ElementTree
from typing import Iterator


class XMLBoardAction(BoardAction, HTTPStream):
    xml_stream_url: str = Field(..., description="URL to XML Stream")
    job_list_xpath: str = Field(
        ..., description="XPath pointing to the job list in the XML stream"
    )

    @property
    def base_url(self):
        return self.xml_stream_url

    @property
    def http_method(self):
        return "GET"

    def get_all_references_from_stream(self):
        input_data = self.pull()
        filtered_data = self.apply_logics(input_data)
        formated_data = map(self.format_switcher, filtered_data)
        references_iter = map(lambda job: job.get("reference"), formated_data)
        references_without_none_iter = filter(
            lambda ref: ref is not None, references_iter
        )
        return references_without_none_iter

    def pull(self) -> Iterator[xml.etree.ElementTree.Element]:
        response = self.send_request()

        if response.status_code >= 400:
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

        xml_stream = response.content
        root_element = xml.etree.ElementTree.fromstring(xml_stream)
        job_list_element = root_element.find(self.job_list_xpath)
        job_list = list(job_list_element)
        return job_list