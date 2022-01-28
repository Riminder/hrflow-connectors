from typing import Iterator, Union, Optional
from pydantic import Field
import requests

from ...core.error import PullError
from ...core.action import PullJobsBaseAction, PushProfileBaseAction
from ...core.auth import AuthorizationAuth, OAuth2PasswordCredentialsBody
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.schemas import HrflowJob, HrflowProfile
from .schemas import WorkableJobModel, WorkableCandidate

logger = get_logger()


class PullJobsAction(PullJobsBaseAction):

    auth: Union[AuthorizationAuth, OAuth2PasswordCredentialsBody]
    subdomain: str = Field(
        ...,
        description="subdomain of a company endpoint in `https://{self.subdomain}.workable.com/spi/v3/jobs` for example subdomain=`eurostar` for eurostar company",
    )

    def pull(self) -> Iterator[WorkableJobModel]:
        """
        pull all jobs from a workable public endpoint jobs stream
        Returns:
            Iterator[WorkableJobModel]: a list of workable job models
        """

        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.auth = self.auth
        pull_jobs_request.url = f"https://{self.subdomain}.workable.com/spi/v3/jobs"
        pull_jobs_request.params = {
            "include_fields": ["description, requirements, benefits, employment_type"]
        }
        prepared_request = pull_jobs_request.prepare()

        # Send Request
        response = session.send(prepared_request)

        if not response.ok:
            raise PullError(
                response,
                message="Failed to get jobs. Check that the subdomain is a valid one",
                subdomain=self.subdomain,
            )

        response_dict = response.json()

        job_list = response_dict["jobs"]
        job_obj_iter = map(WorkableJobModel.parse_obj, job_list)
        return job_obj_iter

    def format(self, data: WorkableJobModel) -> HrflowJob:
        """
        format a job into the hrflow job object format
        Args:
            data (WorkableJobModel): a job object pulled from workable subdomain
        Returns:
            HrflowJob: a job into the hrflow job object format
        """
        job = dict()
        data = data.dict()
        # name and reference
        job["name"] = data.get("title")
        job["reference"] = data.get("shortcode")
        # url
        job["url"] = data.get("url")
        # location
        location = data.get("location")
        location_str = location.get("location_str")
        text = None
        if isinstance(location_str, str):
            text = location_str
        geojson = dict()

        def get_geojson(field_name: str):
            if location.get(field_name) is not None:
                geojson["field_name"] = location.get(field_name)

        get_geojson("country")
        get_geojson("country_code")
        get_geojson("region_code")
        get_geojson("region")
        get_geojson("city")
        get_geojson("zip_code")
        get_geojson("telecommuting")
        job["location"] = dict(text=text, geojson=geojson)
        # sections
        job["sections"] = []

        def create_section(field_name: str):
            title_name = "workable_{}".format(field_name)
            field_value = data.get(field_name)
            if isinstance(field_value, str):
                description = remove_html_tags(field_value)
                section = dict(
                    name=title_name, title=title_name, description=description
                )
                job["sections"].append(section)

        create_section("description")
        create_section("requirements")
        create_section("benefits")
        # creation_date
        job["created_at"] = data.get("created_at")
        # tags
        job["tags"] = []

        def create_tag(field_name):
            name = "workable_{}".format(field_name)
            field_value = data.get("field_name")
            if field_value is not None:
                tag = dict(name=name, value=field_value)
                job["tags"].append(tag)

        create_tag("employment_type")
        create_tag("full_title")
        create_tag("id")
        create_tag("code")
        create_tag("state")
        create_tag("department")
        create_tag("application_url")
        create_tag("shortlink")
        create_tag("employment_type")

        job_obj = HrflowJob.parse_obj(job)

        return job_obj


class PushProfileAction(PushProfileBaseAction):
    auth: Union[AuthorizationAuth, OAuth2PasswordCredentialsBody]
    subdomain: str = Field(
        ...,
        description="subdomain of a company endpoint in `https://{self.subdomain}.workable.com/spi/v3/jobs` for example subdomain=`eurostar` for eurostar company",
    )
    shortcode: str = Field(..., description="The job's shortcode")

    def format(self, data: HrflowProfile) -> WorkableCandidate:
        """
        format [summary]

        Args:
            data (HrflowProfile): [description]

        Returns:
            WorkableCandidate: [description]
        """
        data = data.dict()
        info = data.get("info")
        profile = dict()
        profile["name"] = info.get("full_name")
        profile["summary"] = info.get("summary")
        profile["email"] = info.get("email")
        profile["phone"] = info.get("phone")
        location = info.get("location")
        if isinstance(location.get("text"), str):
            profile["address"] = location.get("text")
        attachments = data.get("attachments")
        if isinstance(attachments, list):
            for attachment in attachments:
                if isinstance(attachment, dict):
                    if attachment["type"] == "resume":
                        profile["resume_url"] = attachment["public_url"]
        candidate_profile = dict(sourced=True, candidate=profile)

        candidate_profile_obj = WorkableCandidate.parse_obj(candidate_profile)
        return candidate_profile_obj

    def push(self, data: WorkableJobModel):
        """
        push [summary]

        Args:
            data (WorkableJobModel): [description]
        """
        profile = next(data)
        profile = profile.dict()

        # Prepare request
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "POST"
        push_profile_request.auth = self.auth
        push_profile_request.url = f"https://{self.subdomain}.workable.com/spi/v3/jobs/{self.shortcode}/candidates"
        push_profile_request.json = profile
        prepared_request = push_profile_request.prepare()

        # Send Request
        response = session.send(prepared_request)

        if not response.ok:
            raise PullError(
                response,
                message="Failed to Push profile.",
            )
