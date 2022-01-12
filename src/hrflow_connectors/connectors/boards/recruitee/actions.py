from typing import Iterator, Dict, Any
from pydantic import Field
from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....utils.logger import get_logger
from ....utils.clean_text import remove_html_tags

logger = get_logger()


class PullJobs(HTTPStream, BoardAction):
    subdomain: str = Field(
        ..., description="the subdomain of your company's careers site."
    )

    @property
    def base_url(self):
        return "https://{}.recruitee.com/api/offers".format(self.subdomain)

    @property
    def http_method(self):
        return "GET"

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        pull all jobs from a recruitee subdomain endpoint
        Raises:
            ConnectionError: if the request failed, you may want to check your subdomain
        Returns:
            Iterator[Dict[str, Any]]: a list of jobs dictionaries
        """
        response = self.send_request()
        if response.ok:
            job_dict_list = response.json()["offers"]
            return job_dict_list
        else:
            logger.error(
                f"Failed to get jobs for company: {self.subdomain}, Check that the subdomain is a valid one"
            )
            error_message = "Unable to pull the data ! Reason: `{}`, `{}`"
            raise ConnectionError(
                error_message.format(response.status_code, response.content)
            )

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a job into the hrflow job object format
        Args:
            data (Dict[str, Any]): a job object pulled from a recruitee company subdomain
        Returns:
            Dict[str, Any]: a job into the hrflow job object format
        """

        job = dict()
        # basic information
        job["name"] = data.get("title")
        job["summary"] = data.get("slug")
        job["url"] = data.get("careers_url")
        job["reference"] = str(data.get("id"))
        # location
        location = data.get("location")
        city = data.get("city")
        country = data.get("country")
        country_code = data.get("country_code")
        geojson = dict(city=city, country=country, country_code=country_code)
        job["location"] = dict(text=location, geojson=geojson, lat=None, lng=None)
        # sections
        description = remove_html_tags(data.get("description"))
        requirements = remove_html_tags(data.get("requirements"))
        job["sections"] = [
            dict(
                name="recruitee_description",
                title="recruitee_description",
                description=description,
            ),
            dict(
                name="recruitee_requirements",
                title="recruitee_requirements",
                description=requirements,
            ),
        ]
        job["created_at"] = data.get("created_at")

        # tags
        remote = str(data.get("remote"))
        category_code = str(data.get("category_code"))
        options_cv = str(data.get("options_cv"))
        min_hours = str(data.get("min_hours"))
        max_hours = str(data.get("max_hours"))
        options_cover_letter = str(data.get("options_cover_letter"))
        experience_code = str(data.get("experience_code"))
        company_name = data.get("company_name")
        department = str(data.get("department"))
        employment_type = str(data.get("employment_type_code"))
        education_code = str(str(data.get("education_code")))
        apply_url = data.get("careers_apply_url")
        job["tags"] = [
            dict(name="recruitee_remote", value=remote),
            dict(name="recruitee_category_code", value=category_code),
            dict(name="recruitee_options_cv", value=options_cv),
            dict(name="recruitee_options_cover_letter", value=options_cover_letter),
            dict(name="recruitee_min_hours", value=min_hours),
            dict(name="recruitee_max_hours", value=max_hours),
            dict(name="recruitee_experience_code", value=experience_code),
            dict(name="recruitee_employment_type", value=employment_type),
            dict(name="recruitee_education_code", value=education_code),
            dict(name="recruitee_company_name", value=company_name),
            dict(name="recruitee_department", value=department),
            dict(name="recruitee_apply_url", value=apply_url),
        ]

        return job
