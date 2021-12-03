from typing import Iterator, Dict, Any

from ....core.auth import OAuth2PasswordCredentialsBody
from ....core.http import HTTPStream
from ....core.action import BoardAction


class GetAllJobs(HTTPStream, BoardAction):
    auth: OAuth2PasswordCredentialsBody

    @property
    def url_base(self):
        return "https://vulcain-eng--recette.my.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/"

    @property
    def http_method(self):
        return "GET"

    def pull(self) -> Iterator[Dict[str, Any]]:
        response = self.send_request()
        if response.status_code == 200:
            return response.json()
        else:
            error_message = "Unable to pull the data ! Reason : `{}`"
            raise ConnectionError(error_message.format(response.content))

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        job = dict()

        # name
        job["name"] = data.get("Name", "Undefined")

        # reference
        # job["name"] = data.get("crtarecr__Reference__c", None)

        # location
        lat = data.get("crta__Location__Latitude__s", None)
        lng = data.get("crta__Location__Longitude__s", None)
        text = data.get("crta__CT_Country__c", "Undefined")
        job["location"] = dict(lat=lat, lng=lng, text=text)

        # sections
        sections = []
        job["sections"] = sections

        # url
        job["url"] = data.get("crtarecr__URL_of_the_form_on_job_offer__c", "")

        # summary
        job["summary"] = data.get("crta__CT_Description__c", "")

        # created_at

        # skills
        job["skills"] = []

        # languages
        job["languages"] = []

        # certifications
        job["certifications"] = []

        # courses
        job["courses"] = []

        # tasks
        job["tasks"] = []

        # tags
        job["tags"] = []

        # metadatas
        job["metadatas"] = []

        # ranges_float
        job["ranges_float"] = []

        # ranges_date
        job["ranges_date"] = []

        return job
