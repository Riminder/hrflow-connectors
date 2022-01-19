from typing import Iterator, Dict, Any, Union, Optional
from pydantic import Field
import requests
import datetime

from ...core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from ...core.action import PullJobsBaseAction, PushProfileBaseAction
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...utils.datetime_converter import from_str_to_datetime

logger = get_logger()


class PullJobsAction(PullJobsBaseAction):

    auth: Union[XAPIKeyAuth, OAuth2PasswordCredentialsBody]
    top: int = Field(
        20, description="show only the first n items, value by default = `20`"
    )
    api_server: str = Field(
        ...,
        description="the API server for your company from the list of API servers for SAP SuccessFactors data centers",
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull all jobs from a local database that uses SAP successfactors API

        Returns:
            Iterator[Dict[str, Any]]: list of all job requisitions with their content
        """
        # Prepare request
        session = requests.Session()
        pull_jobs_request = requests.Request()
        pull_jobs_request.method = "GET"
        pull_jobs_request.url = f"https://{self.api_server}/odata/v2/JobRequisitionLocale?$top={self.top}&expand=jobRequisition"
        pull_jobs_request.auth = self.auth
        pull_jobs_request.headers = {"Accept": "application/json"}
        prepared_request = pull_jobs_request.prepare()

        # Send request
        response = session.send(prepared_request)

        if not response.ok:
            logger.error(f"The request failed !")
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

        logger.info(f"All jobs have been pulled")
        response_dict = response.json()
        job_list = response_dict["d"]["results"]
        return job_list

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format retrieved jobs into a HrFlow job object

        Args:
            data (Dict[str, Any]): job to format

        Returns:
            Dict[str, Any]: job in the HrFlow job object format
        """
        job = dict()

        jobRequisition = data.get("jobRequisition")

        # name
        if data.get("jobTitle") is not None:
            job["name"] = data.get("jobTitle")
        else:
            job["name"] = "Undefined"

        # reference
        job["reference"] = data.get("jobReqId")

        # location
        geojson = dict(
            city=jobRequisition.get("city"),
            country=jobRequisition.get("country"),
            facility=jobRequisition.get("facility"),
            province=jobRequisition.get("stateProvince"),
        )
        job["location"] = dict(
            text=jobRequisition.get("location"),
            city=jobRequisition.get("city"),
            geojson=geojson,
            lat=None,
            lng=None,
        )

        # sections
        job["sections"] = []
        if data.get("jobDescription") is not None:
            description_text = data.get("jobDescription")
            description_text = remove_html_tags(description_text)
            description_text = description_text.replace("#13;", " ")
            description_text = description_text.replace("&", "")
            description_text = description_text.replace("&nbsp;", "")
            description_text = description_text.replace("quo;s", "")

            description_section = dict(
                name="sap_description",
                title="sap_description",
                description=description_text,
            )
            job["sections"].append(description_section)

        # tags
        annual_salary = jobRequisition.get("annual_SA")
        department = jobRequisition.get("department")
        division = jobRequisition.get("division")
        function = jobRequisition.get("function")
        industry = jobRequisition.get("industry")
        monthly_salary = jobRequisition.get("monthly_salary")
        other_bonus = jobRequisition.get("otherBonus")
        salary_base = jobRequisition.get("salaryBase")
        salary_max = jobRequisition.get("salaryMax")
        salary_min = jobRequisition.get("salaryMin")
        job_start_date = jobRequisition.get("jobStartDate")
        job["tags"] = [
            dict(name="sap_annual_SA", value=annual_salary),
            dict(name="sap_department", value=department),
            dict(name="sap_function", value=function),
            dict(name="sap_division", value=division),
            dict(name="sap_industry", value=industry),
            dict(name="sap_monthly_salary", value=monthly_salary),
            dict(name="sap_otherBonus", value=other_bonus),
            dict(name="sap_salaryBase", value=salary_base),
            dict(name="sap_salaryMax", value=salary_max),
            dict(name="sap_salaryMin", value=salary_min),
            dict(name="sap_jobStartDate", value=job_start_date),
        ]
        job["metadatas"] = [
            dict(name="sap_recruiterTeam", value=jobRequisition.get("recruiterTeam")),
            dict(name="sap_sourcerTeam", value=jobRequisition.get("sourcerTeam")),
            dict(
                name="sap_hiringManagerTeam",
                value=jobRequisition.get("hiringManagerTeam"),
            ),
        ]
        return job


class PushProfileAction(PushProfileBaseAction):

    profile_already_exists: Any = Field(
        "COE0019",
        description="an error code that indicates the candidate is already present and to update we need to update each attribute with a unique request",
    )
    auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]
    api_server: str = Field(
        ...,
        description="the API server for your company from the list of API servers for SAP SuccessFactors data centers",
    )

    def format(self, profile: Dict[str, Any]) -> Dict[str, Any]:
        """
        Formats a hrflow profile into a sap successfactors candidate

        Args:
            profile (Dict[str, Any]): profile to format

        Returns:
            Dict[str, Any]: a SAP successfactors profile
        """
        sap_profile = dict()

        # Basic information
        info = profile.get("info")
        sap_profile["address"] = info.get("location").get("text")
        sap_profile["cellPhone"] = info.get("phone")
        fields = info.get("location").get("fields")
        if fields not in [None, []]:
            sap_profile["country"] = fields.get("country")[:-1]
            sap_profile["city"] = fields.get("city")
            sap_profile["zip"] = fields.get("postcode")

        sap_profile["primaryEmail"] = info.get("email")
        sap_profile["firstName"] = info.get("first_name")
        sap_profile["lastName"] = info.get("last_name")
        sap_profile["currentTitle"] = info.get("summary")

        def format_date(date: datetime.datetime) -> str:
            """
            Format `datetime.datetime` to SAP Date

            Args:
                date (datetime.datetime): Datetime to format

            Returns:
                str: formatted date
            """
            timestamp = int(date.timestamp())
            sap_date = f"/Date({timestamp})/"
            return sap_date

        def format_start_date(date_str: str) -> str:
            """
            Format ISO start date to SAP Date

            Args:
                date_str (str): iso date to format

            Returns:
                str: formatted SAP date
            """
            converted_date = from_str_to_datetime(date_str)
            return format_date(converted_date)

        def format_end_date(date_str: str) -> str:
            """
            Format ISO end date to SAP Date

            Args:
                date_str (str): iso date to format

            Returns:
                str: formatted SAP date
            """
            converted_date = from_str_to_datetime(date_str)
            # add 10 seconds to avoid that end_date is less or equal than start_date
            converted_date += datetime.timedelta(seconds=10)
            return format_date(converted_date)

        # educations
        if profile.get("educations") is not None:

            def format_education(education: Dict[str, Any]) -> Dict[str, Any]:
                """
                Format Hrflow education to SAP Education

                Args:
                    education (Dict[str, Any]): Hrflow education

                Returns:
                    Dict[str, Any]: SAP education
                """
                sap_education = dict()
                date_start = education.get("date_start")
                date_end = education.get("date_end")

                if date_start is not None:
                    sap_education["startDate"] = format_start_date(date_start)

                if date_end is not None:
                    sap_education["endDate"] = format_end_date(date_end)

                sap_education["school"] = education.get("school")
                school_location = education.get("location", {})
                school_address = school_location.get("text")
                if school_address is None:
                    school_address = "Undefined"
                sap_education["schoolAddress"] = school_address
                return sap_education

            sap_profile["education"] = dict(results=[])
            for education in profile.get("educations", []):
                sap_education = format_education(education)
                sap_profile["education"]["results"].append(sap_education)

        # experiences
        if profile.get("experiences") is not None:

            def format_experience(experience: Dict[str, Any]) -> Dict[str, Any]:
                """
                Format Hrflow experience to SAP experience

                Args:
                    experience (Dict[str, Any]): Hrflow experience

                Returns:
                    Dict[str, Any]:SAP experience
                """
                sap_experience = dict()
                sap_experience["employer"] = experience.get("company")

                experience_location = experience.get("location", {})
                experience_address = experience_location.get("text")
                if experience_address is None:
                    experience_address = "Undefined"
                sap_experience["employerAddress"] = experience_address

                date_start = experience.get("date_start")
                date_end = experience.get("date_end")
                if date_start is not None:
                    sap_experience["startDate"] = format_start_date(date_start)

                if date_end is not None:
                    sap_experience["endDate"] = format_end_date(date_end)

                return sap_experience

            sap_profile["outsideWorkExperience"] = dict(results=[])
            for experience in profile.get("experiences", []):
                sap_experience = format_experience(experience)
                sap_profile["outsideWorkExperience"]["results"].append(sap_experience)

        return sap_profile

    def push(self, data):
        profile = next(data)

        # Prepare request
        session = requests.Session()
        push_profile_request = requests.Request()
        push_profile_request.method = "POST"
        push_profile_request.url = f"https://{self.api_server}/odata/v2/Candidate"
        push_profile_request.auth = self.auth
        push_profile_request.json = profile
        prepared_request = push_profile_request.prepare()

        # Send request
        response = session.send(prepared_request)
        if self.profile_already_exists in response.content.decode("utf-8"):
            logger.warning("Profile already exists")
        else:
            if not response.ok:
                raise RuntimeError(
                    f"Push profile to sapsuccesfactors api-server: {self.api_server} failed : `{response.content}`"
                )
