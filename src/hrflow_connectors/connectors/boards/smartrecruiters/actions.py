from typing import Iterator, Dict, Any, Optional
from pydantic import Field
import itertools

from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....core.auth import SmartToken
from ....utils.logger import get_logger


logger = get_logger()


class SmartJobs(HTTPStream, BoardAction):
    auth: SmartToken
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
                logger.info(f"Get page of jobs : pageId=`{next_page_id}`")
                response = self.send_request()

                if response.status_code >= 400:
                    error_message = "Unable to pull the data ! Reason : `{}`"
                    raise ConnectionError(error_message.format(response.content))

                logger.info(f"Success to get page of jobs : pageId=`{next_page_id}`")
                response_json = response.json()
                total_found = response_json["totalFound"]  # total jobs found
                next_page_id = response_json["nextPageId"]
                job_list = response_json["content"]
                job_number = len(job_list)
                logger.info(f"{job_number} job(s) got. Total found : {total_found}")
                yield job_list

        # Chain all jobs of each page in an Iterator
        all_chained_job_iter = itertools.chain.from_iterable(get_page())

        return all_chained_job_iter

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        `format`[generates a dictionary of a job attributes, for each job content data]

        Args:
            data (Dict[str, Any]): [unique job offer content data yielded after the function `pull` is executed.]

        Returns:
            Dict[str, Any]: [a job in the HrFlow job object format]
        """
        job = dict()
        # job Title
        job["name"] = data.get("title")
        # job Reference
        job["reference"] = data.get("refNumber")
        # job Url
        job["url"] = None
        # creation date and -update- of the offer
        job["created_at"] = data.get("createdon")
        job["updated_at"] = data.get("updatedon")
        job["summary"] = ""
        # location
        lat = data.get("location", {}).get("latitude")
        lng = data.get("location", {}).get("longitude")
        location_field_list = ["country", "region", "city", "address"]
        location_field_name_list = []
        for field_name in location_field_list:
            if data.get("location", "").get(field_name):
                location_field_name_list.append(
                    data.get("location", "").get(field_name)
                )

        text = " ".join(location_field_name_list)

        job["location"] = dict(lat=lat, lng=lng, text=text)
        # job sections: descriptions and qualifications
        companyDescription = (
            data.get("jobAd", {})
            .get("sections", {})
            .get("companyDescription", {})
            .get("text")
        )
        jobDescription = (
            data.get("jobAd", {})
            .get("sections", {})
            .get("jobDescription", {})
            .get("text")
        )
        qualification = (
            data.get("jobAd", {})
            .get("sections", {})
            .get("qualifications", {})
            .get("text")
        )
        additional_information = (
            data.get("jobAd", {})
            .get("sections", {})
            .get("additionalInformation", {})
            .get("text")
        )
        job["sections"] = [
            dict(
                name="smartrecruiters_company_description",
                title="smartrecruiters_companyDescription",
                description=companyDescription,
            ),
            dict(
                name="smartrecruiters_job_description",
                title="smartrecruiters_jobDescription",
                description=jobDescription,
            ),
            dict(
                name="smartrecruiters_qualifications",
                title="smartrecruiters_qualifications",
                description=qualification,
            ),
            dict(
                name="smartrecruiters_additional_information",
                title="smartrecruiters_additionalInformation",
                description=additional_information,
            ),
        ]
        # job tags
        status = data.get("status")
        posting_status = data.get("postingStatus")
        job_uuid = data.get("id")
        experience_level = data.get("experienceLevel", {}).get("id")
        employmentType = data.get("typeOfEmployment", {}).get("id")
        industry = data.get("industry", {}).get("id")
        creator = data.get(
            "creator",
        )
        function = data.get("function", {}).get("id")
        department = data.get("department", {}).get("id")
        manual = data.get("location", {}).get("manual")
        remote = data.get("location", {}).get("remote")
        eeo_category = data.get("eeoCategory", {}).get("id")
        compensation = data.get("compensation", None)
        job["tags"] = [
            dict(name="smartrecruiters_status", value=status),
            dict(name="smartrecruiters_postingStatus", value=posting_status),
            dict(name="job_uuid", value=job_uuid),
            dict(name="smartrecruiters_experience_level", value=experience_level),
            dict(name="type_of_employment", value=employmentType),
            dict(name="smartrecruiters_compensation", value=compensation),
            dict(name="smartrecruiters_industry", value=industry),
            dict(name="smartrecruiters_creator", value=creator),
            dict(name="smartrecruiters_function", value=function),
            dict(name="smartrecruiters_department-id", value=department),
            dict(name="smartrecruiters_location-manual", value=manual),
            dict(name="smartrecruiters_job-remote", value=remote),
            dict(name="equal_employment_opportunity_category", value=eeo_category),
        ]
        # ranges of duration and salary
        ranges_date = [
            dict(
                name="targetHiringDate",
                value_min=None,
                value_max=data.get("targetHiringDate"),
            )
        ]
        ranges_float = [
            dict(
                name="compensation",
                value_min=data.get("compensation", {}).get("min"),
                value_max=data.get("compensation", {}).get("max"),
                unit=data.get("compensation", {}).get("currency"),
            )
        ]
        job["metadatas"] = dict(
            smartrecruiters_date=ranges_date, smartrecruiters_float=ranges_float
        )

        return job
