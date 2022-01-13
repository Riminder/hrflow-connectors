from typing import Iterator, Dict, Any, Union
from pydantic import Field
import requests
from ...utils.logger import get_logger
from ...utils.clean_text import remove_html_tags
from ...core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from ...core import action as core
import dateutil.parser as dp
from ...utils.hrflow import generate_workflow_response

logger = get_logger()

class PullJobsAction(core.PullJobsAction):

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
        pull all jobs from a local database that uses SAP successfactors API
        Raises:
            ConnectionError if the request fails with the error message
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
            logger.error(f"The request was unsuccessful !")
            error_message = "Unable to pull the data ! Reason : `{}`"
            logger.error(response.content)
            raise ConnectionError(error_message.format(response.content))

        logger.info(f"The request was successful !")
        response_dict = response.json()
        job_list = response_dict["d"]["results"]
        return job_list


    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        format retrieved jobs into a HrFlow job object
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
        # description
        if data.get("jobDescription") is not None:
            description = (
                remove_html_tags(data.get("jobDescription"))
                .replace("#13;", " ")
                .replace("&", "")
                .replace("&nbsp;", "")
                .replace("quo;s", "")
            )
        else:
            description = 'Null'
        job["sections"] = [
            dict(
                name="sap_description", title="sap_description", description=description
            )
        ]
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
            dict(name="sap_annual_salary", value=annual_salary),
            dict(name="sap_department", value=department),
            dict(name="sap_function", value=function),
            dict(name="sap_division", value=division),
            dict(name="sap_industry", value=industry),
            dict(name="sap_monthly_salary", value=monthly_salary),
            dict(name="sap_other_bonus", value=other_bonus),
            dict(name="sap_salary_base", value=salary_base),
            dict(name="sap_salary_max", value=salary_max),
            dict(name="sap_salary_min", value=salary_min),
            dict(name="sap_job_start_date", value=job_start_date),
        ]
        job["metadatas"] = [
            dict(name="sap_recruiter_team", value=jobRequisition.get("recruiterTeam")),
            dict(name="sap_sourcer_team", value=jobRequisition.get("sourcerTeam")),
            dict(
                name="sap_hiring_manager_team",
                value=jobRequisition.get("hiringManagerTeam"),
            ),
        ]
        return job


class PushProfileAction(core.PushProfileAction):

    auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]
    api_server: str = Field(
        ...,
        description="the API server for your company from the list of API servers for SAP SuccessFactors data centers",
    )

    def format(self, profile: Dict[str, Any]) -> Dict[str, Any]:
        """
        formats a hrflow profile into a sap successfactors candidate
        Returns:
            Dict[str, Any]: a json respresentation of a SAP successfactors candidate
        """

        candidate = dict()
        info = profile.get("info")

        candidate["address"] = info.get("location").get("text")
        candidate["cellPhone"] = info.get("phone")
        fields = info.get("location").get("fields")
        if fields not in [None, []]:
            candidate["country"] = fields.get("country")[:-1]
            candidate["city"] = fields.get("city")
            candidate["zip"] = fields.get("postcode")

        candidate["primaryEmail"] = info.get("email")
        candidate["firstName"] = info.get("first_name")
        candidate["lastName"] = info.get("last_name")
        candidate["currentTitle"] = info.get("summary")

        def format_start_date(date):
            return "/Date({})/".format(int(dp.parse(date).timestamp()))

        def format_end_date(date):
            # add 10 seconds to avoid that end_date is less or equal than start_date
            return "/Date({})/".format(int(dp.parse(date).timestamp() + 10))

        if profile.get("educations") is not None:

            def format_education(education):
                result = dict()

                if (
                    education.get("date_end") is not None
                    and education.get("date_start") is not None
                ):
                    result["endDate"] = format_end_date(education.get("date_end"))
                    result["startDate"] = format_start_date(education.get("date_start"))

                result["school"] = education.get("school")
                result["schoolAddress"] = education.get("location").get("text")
                if result["schoolAddress"] is None:
                    result["schoolAddress"] = "Undefined"
                return result

            candidate["education"] = dict(results=[])
            for education in profile.get("educations"):
                candidate["education"]["results"].append(format_education(education))

        if profile.get("experiences") is not None:

            def format_experience(experience):
                result = dict()
                result["employer"] = experience.get("company")
                result["employerAddress"] = experience.get("location").get("text")
                if result["employerAddress"] is None:
                    result["employerAddress"] = "Undefined"
                if (
                    experience.get("date_end") is not None
                    and experience.get("date_start") is not None
                ):
                    result["endDate"] = format_end_date(experience.get("date_end"))
                    result["startDate"] = format_start_date(
                        experience.get("date_start")
                    )
                return result

            candidate["outsideWorkExperience"] = dict(results=[])
            for experience in profile.get("experiences"):
                candidate["outsideWorkExperience"]["results"].append(
                    format_experience(experience)
                )

        return candidate

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

        if not response.ok:
            raise RuntimeError(
                    f"Push profile to sapsuccesfactors api-server: {self.api_server} failed : `{response.content}`"
            )