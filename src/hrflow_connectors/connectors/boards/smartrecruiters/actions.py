from typing import Iterator, Dict, Any, Optional
from pydantic import Field
from ....core.action import BoardAction
import requests


class SmartJobs(BoardAction):
    xstr = lambda s: s or ""
    token: str = Field(..., description="Token to get access to smart jobs pulling API")
    updated_after: Optional[str] = Field(
        ..., description="custom pulling of jobs only updated after a certain date"
    )
    offset: int = Field(
        ...,
        description=" if offset is 0 and limit is 10 and `totalFound` resources are 130, our response will containt the first 10 ",
    )
    posting_status: str = Field("PUBLIC", description="Job offers availability")
    limit: int = Field(..., description="see `offset` description")

    @property
    def base_url(self):
        return "https://api.smartrecruiters.com/jobs"

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        `pull` [send tokenized requests to SmartRecruiters to get job offers data and pull their data]

        Returns:
            [job_datas]: Iterator[Dict[str, Any] [a list of jobs data jsons]

        """
        job_data_list = []
        headers = {"X-SmartToken": self.token}
        params = dict(
            postingStatus=self.posting_status, limit=self.limit, offset=self.offset
        )
        if self.updated_after:
            params["updatedAfter"] = self.updated_after
        response = requests.get(
            url=self.base_url, params=params, headers=headers
        ).json()
        total_found = response["totalFound"]

        while self.offset < total_found:
            params.update({"offset": self.offset})
            response_jobs = requests.get(
                url=self.base_url, params=params, headers=headers
            ).json()
            jobs = response_jobs["content"]
            for job in jobs:
                response_job = requests.get(
                    url="https://api.smartrecruiters.com/jobs/" + job.get("id"),
                    headers=headers,
                ).json()
                job_data_list.append(response_job)

        return job_data_list

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        `format`[generates a dictionary of a job attributes, for each job_son data]

        Args:
            data (Dict[str, Any]): [unique job_data json yielded after the function `pull` is executed.]

        Returns:
            Dict[str, Any]: [a job in the HrFlow job object format]
        """
        job = dict()
        # job Title
        job["name"] = data.get("title")
        # job Reference
        job["references"] = data.get("refNumber")
        # job Url
        job["url"] = None
        # creation date and -update- of the offer
        job["created_at"] = data.get("createdon")
        job["updated_at"] = data.get("updatedon")
        job["summary"] = ""
        # location
        lat = data.get("location").get("latitude")
        lng = data.get("location").get("longitude")
        text = " ".join(
            data.get("location")[key]
            for key in ["country", "region", "city", "address"]
            if data.get("location").get(key)
        )
        job["location"] = dict(lat=lat, lng=lng, text=text)
        # job sections: descriptions and qualifications
        companyDescription = (
            data.get("jobAd").get("sections").get("companyDescription").get("text")
        )
        jobDescription = (
            data.get("jobAd").get("sections").get("jobDescription").get("text")
        )
        qualification = (
            data.get("jobAd").get("sections").get("qualifications").get("text")
        )
        additional_information = (
            data.get("jobAd").get("sections").get("additionalInformation").get("text")
        )
        job["sections"] = [
            dict(
                name="company_description",
                title="companyDescription",
                description=companyDescription,
            ),
            dict(
                name="job_description",
                title="jobDescription",
                description=jobDescription,
            ),
            dict(
                name="qualifications", title="qualifications", description=qualification
            ),
            dict(
                name="additional_information",
                title="additionalInformation",
                description=additional_information,
            ),
        ]
        # language requirements
        language = data.get("jobAd").get("language").get("label")
        job["languages"] = list(dict(name=language, value=None))
        # job tags
        status = data.get("status")
        posting_status = data.get("postingStatus")
        job_uuid = data.get("id")
        experience_level = data.get("experienceLevel", {}).get("id")
        employmentType = data.get("typeOfEmployment", {}).get("id")
        industry = data.get("industry", {}).get("id")
        creator = (
            self.xstr(data.get("creator").get("firstName"))
            + " "
            + self.xstr(data.get("creator").get("lastName"))
        )
        function = data.get("function", {}).get("id")
        department = data.get("department", {}).get("id")
        manual = data.get("location", {}).get("manual")
        remote = data.get("location", {}).get("remote")
        eeo_category = data.get("eeoCategory", {}).get("id")
        compensation = data.get("compensation", {})
        job["tags"] = [
            dict(name="status", value=status),
            dict(name="posting_status", value=posting_status),
            dict(name="job_uuid", value=job_uuid),
            dict(name="experience_level", value=experience_level),
            dict(name="type_of_employment", value=employmentType),
            dict(name="compensation", value=compensation),
            dict(name="industry", value=industry),
            dict(name="creator", value=creator),
            dict(name="function", value=function),
            dict(name="department", value=department),
            dict(name="manual", value=manual),
            dict(name="remote", value=remote),
            dict(name="eeo_category", value=eeo_category),
        ]
        # ranges of duration and salary
        job["ranges_date"] = [
            dict(
                name="targetHiringDate",
                value_min=None,
                value_max=data.get("targetHiringDate"),
            )
        ]
        job["ranges_float"] = [
            dict(
                name="compensation",
                value_min=data.get("compensation", {}).get("min"),
                value_max=data.get("compensation", {}).get("max"),
                unit=data.get("compensation", {}).get("currency"),
            )
        ]
        job["metadatas"] = []

        return job
