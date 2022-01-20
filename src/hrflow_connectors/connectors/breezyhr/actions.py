from typing import Iterator, Dict, Any, Optional
from pydantic import Field
import requests

from ...core.action import PullJobsBaseAction, PushProfileBaseAction
from ...core.auth import OAuth2EmailPasswordBody
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.hrflow import generate_workflow_response
from ...utils.datetime_converter import from_str_to_datetime

logger = get_logger()


class PullJobsAction(PullJobsBaseAction):

    auth: OAuth2EmailPasswordBody
    company_id: Optional[str] = Field(
        None,
        description="ID of company to pull jobs from in Breezy HR database associated with the authenticated user",
    )
    company_name: Optional[str] = Field(
        None, description="the company associated with the authenticated user"
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull jobs from a Taleez jobs owner endpoint
        Returns list of all jobs that have been pulled
        """

        def get_company_id() -> str:
            """
            Get the company id associated with the authenticated user company
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
                logger.debug("Retrieving company id")
                for company in company_list:
                    if company["name"] == self.company_name:
                        return company["_id"]

        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = (
            f"https://api.breezy.hr/v3/company/{get_company_id()}/positions?"
        )
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
        Format a Breezy Hr job object into a hrflow job object

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

        job["location"] = dict(text=address, geojson=geojson, lat=None, lng=None)

        # Sections
        description = remove_html_tags(data.get("description"))
        cleaned_description = description.replace("&nbsp;", " ")
        job["sections"] = [
            dict(
                name="breezy_hr_description",
                title="Breezy_hr_description",
                description=cleaned_description,
            )
        ]
        # tags
        job["tags"] = []

        def create_tag(field_name: str):
            tag_name = "breezy_hr_{}".format(field_name)
            tag_value = data.get(field_name)

            if isinstance(tag_value, dict):
                tag_name_value = tag_value.get("name")
                tag = dict(name=tag_name, value=tag_name_value)
                job["tags"].append(tag)
            if isinstance(tag_value, str):
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


class PushProfileAction(PushProfileBaseAction):

    auth: OAuth2EmailPasswordBody
    company_id: Optional[str] = Field(
        None,
        description="ID of company to pull jobs from in Breezy HR database associated with the authenticated user",
    )
    company_name: Optional[str] = Field(
        None, description="the company associated with the authenticated user"
    )
    position_id: str = Field(
        ..., description="Id of the position to create a new candidate for"
    )
    origin: Optional[str] = Field(
        "sourced",
        description="will indicate in Breezy if the candidate should be marked as sourced or applied",
    )
    cover_letter: Optional[str] = None

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format a Hrflow profile object into a breezy hr profile object

        Args:
            data (Dict[str, Any]): Hrflow Profile to format

        Returns:
            Dict[str, Any]: a BreezyHr formatted profile object
        """

        profile = dict()
        info = data.get("info")
        profile["name"] = info.get("full_name")
        profile["address"] = info.get("location").get("text")
        profile["email_address"] = info.get("email")
        profile["phone_number"] = info.get("phone")
        profile["summary"] = info.get("summary")
        if self.origin is not None:
            profile["origin"] = self.origin

        profile["work_history"] = []

        def format_experiences():

            experiences = data.get("experiences")
            for experience in experiences:
                format_experience = dict()
                format_experience["company_name"] = (
                    experience["company"]
                    if experience["company"] not in ["", None]
                    else "Undefined"
                )
                format_experience["title"] = experience["title"]
                format_experience["summary"] = experience["description"]
                if experience["date_start"] is not None:
                    date_iso = from_str_to_datetime((experience["date_start"]))
                    format_experience["start_year"] = date_iso.year
                    format_experience["start_month"] = date_iso.month
                if experience["date_end"] is not None:
                    date_end_iso = from_str_to_datetime((experience["date_end"]))
                    format_experience["end_year"] = date_end_iso.year
                    format_experience["end_month"] = date_end_iso.month

                profile["work_history"].append(format_experience)

        format_experiences()

        profile["education"] = []

        def format_educations():
            educations = data.get("educations")
            for education in educations:
                format_education = dict()
                if education["school"] == "":
                    education["school"] = "Undefined"
                format_education["school_name"] = education["school"]
                format_education["field_of_study"] = education["title"]
                format_education["start_year"] = education["date_start"]
                format_education["end_year"] = education["date_end"]
                profile["education"].append(format_education)

        format_educations()

        profile["social_profiles"] = []

        def format_urls() -> None:
            """
            format_urls, add links and websites to Taleez profile Social links
            """
            urls = info.get("urls")
            if isinstance(urls, list):
                for url in urls:
                    type = url.get("type")
                    link = url.get("url")
                    if isinstance(link, str):
                        profile["social_profiles"][type] = link
            attachments = info.get("attachments")
            if isinstance(attachments, list):
                for attachment in attachments:
                    file_name = attachment.get("file_name")
                    public_url = attachment.get("public_url")
                    if isinstance(public_url, str):
                        profile["social_profiles"][file_name] = public_url

        format_urls()
        if self.cover_letter is not None:
            profile["cover_letter"] = self.cover_letter

        # add profile skills to tags
        profile["tags"] = []
        skills = data.get("skills")
        if isinstance(skills, list):
            for skill in skills:
                if isinstance(skill, dict):
                    profile["tags"].append(skill["name"])

        return profile

    def push(self, data: Dict[str, Any]) -> None:
        """
        Push a Hrflow profile object to a BreezyHr candidate pool for a position

        Args:
            data (Dict[str, Any]): profile to push
        """
        profile = next(data)
        auth = self.auth
        session = requests.Session()

        def get_company_id() -> str:
            """
            Get the company id associated with the authenticated user company, required for authentification

            Returns:
                str: company id
            """
            if self.company_id is not None:
                return self.company_id
            else:
                get_company_id_request = requests.Request()
                get_company_id_request.method = "GET"
                get_company_id_request.url = "https://api.breezy.hr/v3/companies"
                get_company_id_request.auth = auth
                prepared_request = get_company_id_request.prepare()
                response = session.send(prepared_request)
                if not response.ok:
                    error_message = "Couldn't get company id ! Reason : `{}`"
                    raise RuntimeError(error_message.format(response.content))
                company_list = response.json()
                logger.debug("Retrieving company id")
                for company in company_list:
                    if company["name"] == self.company_name:
                        return company["_id"]

        self.company_id = get_company_id()

        def get_candidate() -> Optional[Dict[str, Any]]:
            """
            Get candidate. If candidate does not exists, return None.

            Returns:
                Optional[Dict[str, Any]]: a dictionary that contains candidate id and name, if candidate doesn't exist it returns None
            """
            verify_candidate_request = requests.Request()
            verify_candidate_request.method = "GET"
            verify_candidate_request.url = f"https://api.breezy.hr/v3/company/{self.company_id}/candidates/search?email_address={profile['email_address']}"
            verify_candidate_request.auth = auth
            prepared_request = verify_candidate_request.prepare()
            response = session.send(prepared_request)
            if not response.ok:
                error_message = "Couldn't get candidate ! Reason : `{}`"
                raise RuntimeError(error_message.format(response.content))
            if response.json() == []:
                return None
            else:
                candidate = response.json()[0]
                return candidate

        candidate = get_candidate()

        if candidate is not None:
            candidate_id = candidate["_id"]
            logger.info(f"Candidate Already exists with the id {candidate_id}")

            def update_profile_request():
                """
                Send a put request to update the candidate profile
                """
                update_candidate_request = requests.Request()
                update_candidate_request.method = "PUT"
                update_candidate_request.url = f"https://api.breezy.hr/v3/company/{self.company_id}/position/{self.position_id}/candidate/{candidate_id}"
                update_candidate_request.auth = auth
                update_candidate_request.headers = {"content-type": "application/json"}
                update_candidate_request.json = profile
                prepared_request = update_candidate_request.prepare()

                response = session.send(prepared_request)
                logger.info("Updating Candidate profile")
                logger.debug(f"`{response.content}`, `{response.status_code}`")
                if not response.ok:
                    error_message = "Couldn't update candidate ! Reason :{}, `{}`"
                    raise RuntimeError(
                        error_message.format(response.status_code, response.content)
                    )

            update_profile_request()

        else:
            # Post profile request
            # Prepare request
            logger.info("Preparing resuest to push candidate profile")
            push_profile_request = requests.Request()
            push_profile_request.method = "POST"
            push_profile_request.url = f"https://api.breezy.hr/v3/company/{self.company_id}/position/{self.position_id}/candidates"
            push_profile_request.auth = auth
            push_profile_request.json = profile
            prepared_request = push_profile_request.prepare()

            # Send request
            response = session.send(prepared_request)
            logger.debug(f"`{response.status_code}`,`{response.content}`")
            if not response.ok:
                raise RuntimeError(
                    f"Push profile to Breezy Hr failed :`{response.status_code}` `{response.content}`"
                )
