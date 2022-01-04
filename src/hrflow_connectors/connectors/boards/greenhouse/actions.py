from typing import Iterator, Dict, Any
from pydantic import Field

from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....core.auth import NoAuth
from ....utils.logger import get_logger


logger = get_logger()


class GetAllJobs(HTTPStream, BoardAction):
    auth: NoAuth
    BOARD_TOKEN: str = Field(
        ...,
        description="Job Board URL token, mandatory to access job boards on `greenhouse.io`: `https://boards-api.greenhouse.io/v1/boards/{board_token}/jobs` getting jobs doesn't require an API Key",
    )

    @property
    def base_url(self):
        return "https://boards-api.greenhouse.io/v1/boards/{}/jobs".format(
            self.BOARD_TOKEN
        )

    @property
    def http_method(self):
        return "GET"

    def pull(self) -> Iterator[Dict[str, Any]]:

        self.params["content"] = True
        response = self.send_request()
        if response.status_code == 200:
            job_dict = response.json()
            TOTAL_JOB = job_dict["meta"]["total"]
            logger.info(f"total jobs found: {TOTAL_JOB}")
            job_list = job_dict["jobs"]
            return job_list
        else:
            logger.error(f"Failed to get jobs from board: {self.BOARD_TOKEN}")
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:

        job = dict()

        job["name"] = data.get("title")
        job["reference"] = data.get("internal_job_id")
        job["url"] = data.get("absolute_url")
        location = data.get("location").get("name")
        job["location"] = dict(text=location, lat=None, lng=None)
        description = data.get("content")
        job["sections"] = dict(
            name="greenhouse_description",
            title="greenhouse_description",
            description=description,
        )

        return job
