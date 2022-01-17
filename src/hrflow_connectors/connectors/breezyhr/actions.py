from typing import Iterator, Dict, Any, Optional
from pydantic import Field
import requests
from ...core import action as core
from ...core.auth import OAuth2EmailPasswordBody
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.hrflow import generate_workflow_response

logger = get_logger()


class PullJobsAction(core.PullJobsAction):
    auth: OAuth2EmailPasswordBody
    company_id: Optional[int]= Field(None, description="ID of company to pull jobs from in Breezy HR database associated with the authenticated user")
    company_name: Optional[str]= Field(None, description="the company associated with the authenticated user")

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull jobs from a Taleez jobs owner endpoint
        Returns list of all jobs that have been pulled
        """
        def get_company_id():
            """
            getting the company id associated with the authenticated user company

            """
            if self.company_id is not None:
                return self.company_id
            else:
                get_company_id_request = requests.Request()
                get_company_id_request.method = "GET"
                get_company_id_request.url = "https://api.breezy.hr/v3/companies"
                get_company_id_request.auth = self.auth
                prepared_request = get_company_id_request.prepare()
                response = session.send(prepared_request)
                if not response.ok:
                    error_message = "Couldn't get company id ! Reason : `{}`"
                    raise RuntimeError(error_message.format(response.content))
                company_list = response.json()
                logger.debug("CRetrieving company id")
                for company in company_list:
                    if company['name'] == self.company_name:
                        return company["_id"]

        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = f"https://api.breezy.hr/v3/company/{get_company_id()}/positions?"
        pull_jobs_request.auth = self.auth
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            logger.error(f"Failed to get jobs from this endpoint")
            error_message = "Unable to pull the data ! Reason : `{}` `{}`"
            raise RuntimeError(
                error_message.format(response.status_code, response.content)
            )

        return response.json()

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format a Breezy Hr job object into a hrflow job object

        Returns:
            Dict[str, Any]: a job object in the hrflow job format
        """
        job = dict()

        # Basic information
        job["name"] = data.get("name")
        logger.info(job["name"])
        job["reference"] = data.get("_id")
        logger.info(job["reference"])
        job["summary"] = None

        # Location
        location = data.get("location")
        country = location.get("country")
        country_name = country.get("name")
        city = location.get("city")
        address = location.get("name")
        geojson = dict(country=country_name, city=city)
        
        job["location"] = dict(text=address,geojson=geojson, lat=None, lng=None)

        # Sections
        description = remove_html_tags(data.get("description"))
        cleaned_description = description.replace("&nbsp;", " ")
        job['sections'] = [
            dict(name="breezy_hr_description", title="Breezy_hr_description", description=cleaned_description)
        ]
        #tags
        job["tags"] = []
        def create_tag(field_name: str):
            tag_name = "breezy_hr_{}".format(field_name)
            tag_value = data.get(field_name)

            if isinstance(tag_value,dict):
                tag_name_value = tag_value.get('name')
                tag = dict(name=tag_name, value=tag_name_value)
                job["tags"].append(tag)
            if isinstance(tag_value,str):
                tag = dict(name=tag_name, value=tag_value)
                job["tags"].append(tag)

        create_tag("type")
        create_tag("experience")
        create_tag("education")
        create_tag("department")
        create_tag("requisition_id")
        create_tag("category")
        create_tag("candidate_type")
        is_remote = dict(name="breezy_hr_remote", value=location.get("is_remote"))
        job["tags"].append(is_remote)

        job["created_at"] = data.get("creation_date")
        job["updated_at"] = data.get("updated_date")

        job["metadatas"] = data.get("tags")
        return job

        
        
        

