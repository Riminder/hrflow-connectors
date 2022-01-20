from typing import Iterator, Dict, Any, Optional
from pydantic import Field
import itertools
import requests

from ...core.action import PullJobsBaseAction, PushProfileBaseAction
from ...core.auth import XSmartTokenAuth
from ...utils.logger import get_logger


logger = get_logger()


class PullJobsAction(PullJobsBaseAction):
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

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull all jobs from SmartRecruiters

        Returns:
            Iterator[Dict[str, Any]]: an iterator of jobs
        """
        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = "https://api.smartrecruiters.com/jobs"
        pull_jobs_request.auth = self.auth

        ## Set params
        # If param value is `None`, `requests` ignores the param
        pull_jobs_params = dict()
        pull_jobs_params["q"] = self.query
        pull_jobs_params["updatedAfter"] = self.updated_after
        pull_jobs_params["postingStatus"] = self.posting_status
        pull_jobs_params["status"] = self.job_status
        pull_jobs_params["limit"] = self.limit
        pull_jobs_request.params = pull_jobs_params

        # Define page generator
        def get_page():
            next_page_id = None
            job_list = None
            while job_list != []:
                pull_jobs_request.params["pageId"] = next_page_id
                prepared_request = pull_jobs_request.prepare()
                response = session.send(prepared_request)

                if not response.ok:
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

            # prepare `get_job` request
            get_job_request = requests.Request()
            get_job_request.method = "GET"
            get_job_request.url = f"https://api.smartrecruiters.com/jobs/{job_id}"
            get_job_request.auth = self.auth

            # send request
            prepared_request = pull_jobs_request.prepare()
            response = session.send(prepared_request)

            if not response.ok:
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


class PushProfileAction(PushProfileBaseAction):
    auth: XSmartTokenAuth
    job_id: str = Field(
        ...,
        description="Id of a Job to which you want to assign a candidate when it’s created. A profile is sent to this URL `https://api.smartrecruiters.com/jobs/{job_id}/candidates` ",
    )

    def format(self, profile: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format a profile hrflow object to a smartrecruiters profile object

        Args:
            profile (Dict[str, Any]): [profile object in the hrflow profile format]

        Returns:
            Dict[str, Any]: [profile in the SmartRecruiters candidate application format]
        """

        value_or_undefined = lambda s: s or "Undefined"

        def format_project(project):
            formatted_project_dict = dict()
            # current
            formatted_project_dict["current"] = False
            # start date
            start_datetime_str = project.get("date_start")
            if start_datetime_str is None:
                # datetime is either in format YYYY or YYYY-MM...
                # if there is none we force this value to avoid conflict with smartrecruiters profile object
                start_date = "XXXX"
            else:
                start_date = start_datetime_str.split("T")[0]

            formatted_project_dict["startDate"] = start_date

            end_datetime_str = project.get("date_end")
            if end_datetime_str is None:
                end_date = "XXXX"
            else:
                end_date = end_datetime_str.split("T")[0]

            formatted_project_dict["endDate"] = end_date
            formatted_project_dict["location"] = value_or_undefined(
                project["location"]["text"]
            )
            formatted_project_dict["description"] = project["description"]

            return formatted_project_dict

        def format_educations(educations):
            formatted_education_list = []

            for education_entity in educations:
                formatted_education = format_project(education_entity)
                if education_entity.get("school") is None:
                    formatted_education["instituion"] = "Undefined"
                else:
                    formatted_education["institution"] = education_entity.get("school")

                if education_entity.get("title") is None:
                    formatted_education["degree"] = "Undefined"
                else:
                    formatted_education["degree"] = education_entity.get("title")
                formatted_education["major"] = "Undefined"

                formatted_education_list.append(formatted_education)

            return formatted_education_list

        def format_experiences(experiences):
            formatted_experience_list = []

            for exp in experiences:
                formatted_exp = format_project(exp)
                if exp["title"] is None:
                    formatted_exp["title"] = "Undefined"
                else:
                    formatted_exp["title"] = exp["title"]
                if exp["company"] is None:
                    formatted_exp["company"] = "Undefined"
                else:
                    formatted_exp["company"] = exp["company"]

                formatted_experience_list.append(formatted_exp)

            return formatted_experience_list

        info = profile["info"]
        smart_candidate = dict()
        smart_candidate["firstName"] = info["first_name"]
        smart_candidate["lastName"] = info["last_name"]
        smart_candidate["email"] = info["email"]
        smart_candidate["phoneNumber"] = info["phone"]

        if info["location"]["fields"] not in [
            [],
            None,
        ]:  # check if fields is not an undefined list
            smart_candidate["location"] = dict(
                city=value_or_undefined(info["location"]["fields"].get("city", {})),
                country=value_or_undefined(
                    info["location"]["fields"].get("country", {})
                ),
                region=value_or_undefined(info["location"]["fields"].get("state", {})),
                lat=info["location"]["lat"]
                if info["location"]["lat"] is not None
                else 0,
                lng=info["location"]["lng"]
                if info["location"]["lng"] is not None
                else 0,
            )
        else:
            smart_candidate["location"] = dict(
                country="Undefined",
                region="Undefined",
                City="Undefined",
                lat=0,
                lng=0,
            )

        smart_candidate["web"] = dict(info["urls"])
        smart_candidate["tags"] = []
        smart_candidate["education"] = format_educations(profile.get("educations"))
        smart_candidate["experience"] = format_experiences(profile.get("experiences"))
        smart_candidate["attachments"] = profile.get("attachments")
        smart_candidate["consent"] = True

        return smart_candidate

    def push(self, data: Dict[str, Any]):
        """
        Push profile

        Args:
            data (Dict[str, Any]): Profile
        """
        profile = next(data)

        # Prepare request
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "POST"
        push_profile_request.url = (
            f"https://api.smartrecruiters.com/jobs/{self.job_id}/candidates"
        )
        push_profile_request.auth = self.auth
        push_profile_request.json = profile
        prepared_request = push_profile_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            raise RuntimeError(
                f"Push profile to SmartRecruiters failed : `{response.content}`"
            )
