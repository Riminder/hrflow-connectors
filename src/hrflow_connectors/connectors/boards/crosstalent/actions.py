from typing import Iterator, Dict, Any

from ....core.auth import OAuth2PasswordCredentialsBody
from ....core.http import HTTPStream
from ....core.action import BoardAction
from ....core.hrflow_wrapper import JobWrapper
from .formats.job_format import JobFormat

from pydantic import Field


class GetAllJobs(HTTPStream, BoardAction):
    auth: OAuth2PasswordCredentialsBody
    subdomain: str = Field(
        ...,
        description="Subdomain Crosstalent just before `salesforce.com`. For example subdomain=`my_subdomain.my` in `http://my_subdomain.my.salesforce.com/ABC`",
    )

    @property
    def url_base(self):
        return "https://{}.salesforce.com/services/apexrest/crta/HrFlowGetJobOffers/".format(
            self.subdomain
        )

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
        job_params = dict()

        # name
        job_params["name"] = data.get("Name", "Undefined")

        # reference
        # TODO remove comment after debug format
        # job["name"] = data.get("Id")

        # created_at
        job_params["created_at"] = data.get("CreatedDate")

        # url
        job_params["url"] = data.get("crtarecr__URL_of_the_form_on_job_offer__c")

        # location
        lat = data.get("crta__Location__Latitude__s")
        lng = data.get("crta__Location__Longitude__s")
        text = data.get("Lieu__c")

        gmaps = dict()
        gmaps["city"] = data.get("crta__CT_City__c")
        gmaps["country"] = data.get("crta__CT_Country__c")

        postcode = None
        postcode = data.get("crta__CT_Postal_code__c")
        if postcode is None:
            postcode = data.get("crta__Postal_Code__c")

        gmaps["postcode"] = postcode
        gmaps["state"] = data.get("crta__Etat__c")

        job_params["location"] = dict(lat=lat, lng=lng, text=text, gmaps=gmaps)

        # summary
        job_params["summary"] = data.get("crta__CT_Description__c")

        ## Instanciate job wrapper
        # it makes it easier for us to use the job object
        job_object = JobWrapper(**job_params)

        # sections
        section_id_list = JobFormat.sections
        for section_id in section_id_list:
            section_description = data.get(section_id)
            if section_description is not None:
                job_object.set_section(
                    name=section_id, title=section_id, description=section_description
                )

        # languages
        job_object.languages = []
        language_name_1 = data.get("crtarecr__Language_1__c")
        language_level_1 = data.get("crtarecr__Language_level_1__c")
        if language_name_1 is not None:
            job_object.add_field(
                job_object.languages, name=language_name_1, value=language_level_1
            )

        language_name_2 = data.get("crtarecr__Language_2__c")
        language_level_2 = data.get("crtarecr__Language_level_2__c")
        if language_name_2 is not None:
            job_object.add_field(
                job_object.languages, name=language_name_2, value=language_level_2
            )

        language_name_3 = data.get("crtarecr__Language_3__c")
        language_level_3 = data.get("crtarecr__Language_level_3__c")
        if language_name_3 is not None:
            job_object.add_field(
                job_object.languages, name=language_name_3, value=language_level_3
            )

        # tags
        tag_id_list = JobFormat.tags
        for tag_id in tag_id_list:
            tag_value = data.get(tag_id)
            if tag_value is not None:
                job_object.set_tag(name=tag_id, value=tag_value)

        # metadatas
        meta_id_list = JobFormat.metadatas
        for meta_id in meta_id_list:
            meta_value = data.get(meta_id)
            if meta_value is not None:
                job_object.set_meta(name=meta_id, value=meta_value)

        return job_object.dict()
