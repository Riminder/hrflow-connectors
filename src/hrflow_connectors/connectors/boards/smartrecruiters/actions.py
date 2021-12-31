from typing import Iterator, Dict, Any, Optional
from pydantic import Field
import itertools
import requests

from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....core.auth import XSmartTokenAuth
from ....utils.logger import get_logger


logger = get_logger()


class SmartJobs(HTTPStream, BoardAction):
    auth: XSmartTokenAuth
    query: Optional[str] = Field(
        None,
        description="Full-text search query based on a job title; case insensitive; e.g. java developer",
    )
    updated_after: Optional[str] = Field(
        None, description="ISO8601-formatted time boundaries for the job update time"
    )
    posting_status: Optional[str] = Field(
        None,
        description="Posting status of a job. Available values : PUBLIC, INTERNAL, NOT_PUBLISHED, PRIVATE",
    )
    job_status: Optional[str] = Field(
        None,
        description="Status of a job. Available values : CREATED, SOURCING, FILLED, INTERVIEW, OFFER, CANCELLED, ON_HOLD",
    )
    limit: int = Field(
        10,
        description="Number of elements to return per page. max value is 100. Default value : 10",
    )

    @property
    def base_url(self):
        return "https://api.smartrecruiters.com/jobs"

    @property
    def http_method(self):
        return "GET"

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull all jobs from SmartRecruiters

        Returns:
            Iterator[Dict[str, Any]]: an iterator of jobs
        """
        # If param value is `None`, `requests` in `HTTPStream` ignores the param
        self.params["q"] = self.query
        self.params["updatedAfter"] = self.updated_after
        self.params["postingStatus"] = self.posting_status
        self.params["status"] = self.job_status
        self.params["limit"] = self.limit

        # Define page generator
        def get_page():
            next_page_id = None
            job_list = None
            while job_list != []:
                self.params["pageId"] = next_page_id
                response = self.send_request()

                if response.status_code >= 400:
                    logger.error(f"Fail to get page of jobs : pageId=`{next_page_id}`")
                    error_message = "Unable to pull the data ! Reason : `{}`"
                    raise ConnectionError(error_message.format(response.content))

                logger.info(f"Get page of jobs : pageId=`{next_page_id}`")
                response_json = response.json()
                total_found = response_json["totalFound"]  # total jobs found
                next_page_id = response_json["nextPageId"]
                job_list = response_json["content"]
                job_number = len(job_list)
                logger.info(f"{job_number} job(s) got. Total found : {total_found}")
                yield job_list

        # Chain all jobs of each page in an Iterator
        chained_light_job_iter = itertools.chain.from_iterable(get_page())

        # `all_chained_job_iter` contains data-reduced jobs.
        # This is a feature of the `GET /jobs` search request.
        # We will retrieve all the information for each job by requesting it
        # from SmartRecruiter `GET /jobs/{jobId}`.

        def get_full_job(light_job_dict: Dict[str, Any]) -> Dict[str, Any]:
            """
            Get full job with all information available for the job  `light_job_dict`

            This function use `id` in `light_job_dict` to send request to SmartRecuiter and
            get all information of this job.

            Args:
                light_job_dict (Dict[str, Any]): light job

            Returns:
                Dict[str, Any]: full job
            """
            job_id = light_job_dict["id"]
            hearders = dict()
            self.auth.update(headers=hearders)
            get_job_url = f"https://api.smartrecruiters.com/jobs/{job_id}"
            response = requests.get(get_job_url, headers=hearders)

            if response.status_code >= 400:
                logger.error(f"Fail to get full job id=`{job_id}`")
                error_message = f"Unable to pull the job id=`{job_id}` ! Reason : `{response.content}`"
                raise ConnectionError(error_message)

            logger.info(f"Get full job id=`{job_id}`")
            return response.json()

        chained_full_job_iter = map(get_full_job, chained_light_job_iter)

        return chained_full_job_iter

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format a job from SmartRecruiter job format to Hrflow job format

        Args:
            data (Dict[str, Any]): Job offer

        Returns:
            Dict[str, Any]: Job in the HrFlow job object format
        """
        job = dict()

        # job Title
        job["name"] = data.get("title", "Undefined")

        # job Reference
        job["reference"] = data.get("refNumber")

        # job Url
        job["url"] = None

        # creation date and update date of the offer
        job["created_at"] = data.get("createdon")
        job["updated_at"] = data.get("updatedon")
        job["summary"] = None

        # location
        lat = data.get("location", {}).get("latitude")
        if lat is not None:
            lat = float(lat)

        lng = data.get("location", {}).get("longitude")
        if lng is not None:
            lng = float(lng)

        location_field_list = ["country", "region", "city", "address"]
        location_field_name_list = []
        for field_name in location_field_list:
            field = data.get("location", {}).get(field_name)
            if field is not None:
                location_field_name_list.append(field)

        text = " ".join(location_field_name_list)

        job["location"] = dict(lat=lat, lng=lng, text=text)

        # job sections: descriptions and qualifications
        job["sections"] = []
        job_ad_section_list = data.get("jobAd", {}).get("sections", {})

        def add_section(section_name: str):
            """
            Add section in Hrflow job

            Args:
                section_name (str): section name
            """
            name_prefix = "smartrecruiters_jobAd-sections"
            job_ad_section = job_ad_section_list.get(section_name)
            if job_ad_section is not None:
                name = f"{name_prefix}-{section_name}"
                title = job_ad_section.get("title")
                description = job_ad_section.get("text")
                section = dict(name=name, title=title, description=description)
                job["sections"].append(section)

        add_section("companyDescription")
        add_section("jobDescription")
        add_section("qualifications")
        add_section("additionalInformation")

        # job tags
        status = data.get("status")
        posting_status = data.get("postingStatus")
        job_id = data.get("id")
        experience_level_id = data.get("experienceLevel", {}).get("id")
        employmentType_id = data.get("typeOfEmployment", {}).get("id")
        industry_id = data.get("industry", {}).get("id")
        creator = data.get("creator", {})
        function_id = data.get("function", {}).get("id")
        department_id = data.get("department", {}).get("id")
        manual = data.get("location", {}).get("manual")
        remote = data.get("location", {}).get("remote")
        eeo_category_id = data.get("eeoCategory", {}).get("id")
        compensation = data.get("compensation", {})
        job["tags"] = [
            dict(name="smartrecruiters_status", value=status),
            dict(name="smartrecruiters_postingStatus", value=posting_status),
            dict(name="smartrecruiters_id", value=job_id),
            dict(name="smartrecruiters_experienceLevel-id", value=experience_level_id),
            dict(name="smartrecruiters_typeOfEmployment-id", value=employmentType_id),
            dict(
                name="smartrecruiters_compensation-min", value=compensation.get("min")
            ),
            dict(
                name="smartrecruiters_compensation-max", value=compensation.get("max")
            ),
            dict(
                name="smartrecruiters_compensation-currency",
                value=compensation.get("currency"),
            ),
            dict(name="smartrecruiters_industry-id", value=industry_id),
            dict(
                name="smartrecruiters_creator-firstName", value=creator.get("firstName")
            ),
            dict(
                name="smartrecruiters_creator-lastName", value=creator.get("lastName")
            ),
            dict(name="smartrecruiters_function-id", value=function_id),
            dict(name="smartrecruiters_department-id", value=department_id),
            dict(name="smartrecruiters_location-manual", value=manual),
            dict(name="smartrecruiters_location-remote", value=remote),
            dict(name="smartrecruiters_eeoCategory-id", value=eeo_category_id),
            dict(
                name="smartrecruiters_targetHiringDate",
                value=data.get("targetHiringDate"),
            ),
        ]

        return job
