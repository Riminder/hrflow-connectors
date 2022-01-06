from ....core.action import CatchProfile
import xml.etree.ElementTree
from ....utils.logger import get_logger
from ....utils.hrflow import generate_workflow_response

from pydantic import Field
from typing import Iterator, Union, Dict, Any

TalentDataType = Union[str, xml.etree.ElementTree.Element, Dict[str, Any]]
logger = get_logger()


class PullProfile(CatchProfile):

    def format(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format the input data into a push-ready data schema
        WARNING: If you want to map the format function to rewrite a pipeline of an `Action` from a connector,
        you should use the function `format_switcher`, not the `format` of the parent class.
        If you take `format` from the parent class, then `format_function_name` will be ignored
        and only `format` will be used to format the data.
        Args:
            request (Dict[str, Any]): body we want to adapt to the output format
        Returns:
            Dict[str, Any]: parameters to put in the parsing endpoint
        """

        def get_binary_resume(FileContents):
            byte_array = bytearray(FileContents)
            binary_resume = bytes(byte_array)
            return binary_resume

        hrflow_tags = [{
                "name": "JobRefID",
                "value": request["JobRefID"]
            }]
        output_data = {
            "source_key": self.source_key,
            "profile_file": get_binary_resume(self.request["FileContents"]),
            "tags": hrflow_tags
        }
        return output_data
