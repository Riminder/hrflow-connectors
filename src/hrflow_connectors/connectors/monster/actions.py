from ...core import action as core
import xml.etree.ElementTree
from ...utils.logger import get_logger

from typing import Union, Dict, Any

TalentDataType = Union[str, xml.etree.ElementTree.Element, Dict[str, Any]]
logger = get_logger()


class CatchProfileAction(core.CatchProfileAction):

    def format(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format the input data into a push-ready data schema
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