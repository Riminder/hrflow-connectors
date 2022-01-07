from typing import Iterator, Dict, Any, Union
from pydantic import Field
from ....core.action import BoardAction
from ....core.http import HTTPStream
from ....utils.logger import get_logger
from ....utils.clean_text import remove_html_tags
from ....core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth

logger = get_logger()


class PullJobs(HTTPStream, BoardAction):
    auth: Union[XAPIKeyAuth, OAuth2PasswordCredentialsBody]
    top: int = Field(
        20, description="show only the first n items, value by default = `20`"
    )
    subdomain: str = Field(
        ...,
        description="the API server for your company from the list of API servers for SAP SuccessFactors data centers",
    )

    @property
    def base_url(self):
        return "https://{}/odata/v2/JobRequisitionLocale?$top={}&expand=jobRequisition".format(
            self.subdomain, self.top
        )

    @property
    def http_method(self):
        return "GET"

    def build_request_headers(self):
        super().build_request_headers()
        self.headers["Accept"] = "application/json"

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        pull all jobs from a local database that uses SAP successfactors API

        Raises:
            ConnectionError if the request fails with the error message

        Returns:
            Iterator[Dict[str, Any]]: list of all job requisitions with their content
        """

        response = self.send_request()
        if response.ok:
            logger.info(f"The request was successful !")
            job_dict_list = response.json()["d"]["results"]
            logger.info(f"Getting all the results totaling: {len(job_dict_list)}")
            return job_dict_list
        else:
            logger.error(f"The request was unsuccessful !")
            error_message = "Unable to pull the data ! Reason : `{}`"
            logger.error(response.content)
            raise ConnectionError(error_message.format(response.content))

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
            description = None
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
