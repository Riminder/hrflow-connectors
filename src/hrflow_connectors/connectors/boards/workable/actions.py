from typing import Iterator, Dict, Any
from pydantic import Field
from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....utils.logger import get_logger
from ....utils.clean_text import remove_html_tags

logger = get_logger()


class PullJobs(HTTPStream, BoardAction):
    subdomain: str = Field(
        ...,
        description="subdomain of a company endpoint in `https://www.workable.com/api/accounts/{subdomain}` for example subdomain=`eurostar` for eurostar company",
    )

    @property
    def base_url(self):
        return "https://www.workable.com/api/accounts/{}".format(self.subdomain)

    @property
    def http_method(self):
        return "GET"

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        pull all jobs from a workable public endpoint jobs stream

        Raises:
            Exception: if there are no jobs posted
            ConnectionError: if the request failed, you may want to check your subdomain

        Returns:
            Iterator[Dict[str, Any]]: a list of jobs dictionaries
        """

        response = self.send_request()
        if response.status_code == 200:
            job_dict_list = response.json()["jobs"]
            total_found = len(job_dict_list)
            if total_found == 0:
                logger.info(f"No jobs found for this request")
                raise Exception(
                    f"This company has no jobs available on their workable public endpoint"
                )
            else:
                logger.info(f"Total jobs found for this request: {len(job_dict_list)}")
                return job_dict_list
        else:
            logger.error(
                f"Failed to get jobs from subdomain: {self.subdomain}. Check that the subdomain is a valid one"
            )
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a job into the hrflow job object format

        Args:
            data (Dict[str, Any]): a job object pulled from workable subdomain

        Returns:
            Dict[str, Any]: a job into the hrflow job object format
        """
        job = dict()
        #name and reference
        job["name"] = data.get("title")
        job["reference"] = data.get("shortcode")
        #location
        country = data.get("country")
        state = data.get("state")
        city = data.get("city")
        job["location"] = dict(
            text=city, country=country, state=state, lat=None, lng=None
        )
        #url
        job["url"] = data.get("url")
        #sections
        description = remove_html_tags(data.get("description"))
        job["sections"] = [
            dict(
                name="workable_description",
                title="workable_description",
                description=description,
            )
        ]
        #creation_date
        job["created_at"] = data.get("created_at")
        #tags
        employment_type = data.get("employment_type")
        industry = data.get("industry")
        function = data.get("function")
        experience = data.get("experience")
        education = data.get("education")
        application_url = data.get("application_url")
        department = data.get("department")
        job["tags"] = [
            dict(name="workable_employment_type", value=employment_type),
            dict(name="workable_indsutry", value=industry),
            dict(name="workable_function", value=function),
            dict(name="workable_experience", value=experience),
            dict(name="workable_education", value=education),
            dict(name="workable_application_url", value=application_url),
            dict(name="workable_department", value=department),
        ]
        job["metadatas"] = []

        return job
