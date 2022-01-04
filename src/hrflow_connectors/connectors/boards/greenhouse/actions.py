from typing import Iterator, Dict, Any
from pydantic import Field
import html
from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....utils.logger import get_logger
from ....utils.clean_text import remove_html_tags

logger = get_logger()


class GetAllJobs(HTTPStream, BoardAction):
    board_token: str = Field(
        ...,
        description="Job Board URL token, which is usually the company `name` -for example `lyft`- when it has job listings on greenhouse, mandatory to access job boards on `greenhouse.io`: `https://boards-api.greenhouse.io/v1/boards/{board_token}/jobs`, getting jobs doesn't require an API Key",
    )

    @property
    def base_url(self):
        return (
            "https://boards-api.greenhouse.io/v1/boards/{}/jobs/?content=true".format(
                self.board_token
            )
        )

    @property
    def http_method(self):
        return "GET"

    def pull(self) -> Iterator[Dict[str, Any]]:
        """pull : sends a request to get all jobs from a greenhouse job board

        Returns:
            Iterator[Dict[str, Any]]: list of all jobs with their content if available
        """

        response = self.send_request()
        if response.status_code == 200:
            job_dict = response.json()
            total_info = job_dict["meta"]["total"]
            job_list = job_dict["jobs"]
            job_number = len(job_list)
            logger.info(f"{job_number} job(s) got. Total found : {total_info}")
            return job_list
        else:
            logger.error(f"Failed to get jobs from board: {self.board_token}")
            logger.debug(f"Check that your board token: {self.board_token} is valid")
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """format : convert every job pulled from greenhouse job board into a HrFlow job object

        Returns:
            Dict[str, Any]: job in the HrFlow job object format
        """

        job = dict()
        job["name"] = data.get("title")
        job["summary"] = None
        job["reference"] = str(data.get("internal_job_id"))
        job["url"] = data.get("absolute_url")
        location = data.get("location").get("name")
        job["location"] = dict(text=location, lat=None, lng=None)

        description_content = data.get("content")

        text = remove_html_tags(html.unescape(description_content))

        job["sections"] = [
            dict(
                name="greenhouse_description",
                title="greenhouse_description",
                description=text,
            )
        ]
        job["metadata"] = data.get("metadata")

        department_name = data.get("departments")
        office_name = data.get("offices")
        education = data.get("education_optional")
        employment = data.get("employment")

        job["tags"] = [
            dict(name="greenhouse_department", value=department_name),
            dict(name="greenhouse_office", value=office_name),
            dict(name="greenhouse_education", value=education),
            dict(name="greenhouse_employment", value=employment),
        ]

        job["updated_at"] = data.get("updated_at")

        return job
