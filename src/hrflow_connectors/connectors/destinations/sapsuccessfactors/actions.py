from ....core.action import ProfileDestinationAction
from ....core.http import HTTPStream
from ....core.auth import OAuth2PasswordCredentialsBody, XAPIKeyAuth
from pydantic import Field
from typing import Dict, Any, Optional, Union, List
import dateutil.parser as dp

class PushProfile(ProfileDestinationAction, HTTPStream):

    auth: Union[OAuth2PasswordCredentialsBody, XAPIKeyAuth]
    payload: Dict[str, Any] = dict()
    subdomain: str = Field(
        ...,
        description="the API server for your company from the list of API servers for SAP SuccessFactors data centers",
    )


    def build_request_headers(self):
        super().build_request_headers()
        self.headers["content-type"] = "application/json"
        self.headers["Accept"] = "application/json"

    @property
    def base_url(self):
        return "https://{}/odata/v2/Candidate".format(self.subdomain)

    @property
    def http_method(self):
        return "POST"

    def format(self,profile:Dict[str, Any]) -> Dict[str, Any]:

        candidate = dict()
        info = profile.get('info')

        candidate['address'] = info.get('location').get('text')
        candidate['cellPhone'] = info.get('phone')
        fields = info.get('location').get('fields')
        if fields not in [None, []]:
            candidate['country'] = fields.get('country')
            if candidate['country'] == 'FRA':
                candidate['country'] = 'FR'
            if candidate['country'] == 'USA':
                candidate['country'] = 'US'
            candidate['city'] = fields.get('city')
            candidate['zip'] = fields.get('postcode')

        candidate['primaryEmail'] = info.get('email')
        candidate['firstName'] = info.get('first_name')
        candidate['lastName'] = info.get('last_name')
        candidate['currentTitle'] = info.get('summary')

        def formate_date_time(date):
            return "/Date({}000)/".format(int(dp.parse(date).timestamp()))

        if profile.get('educations') is not None:
            def format_education(education):
                result = dict()

                if education.get('date_end') is not None and education.get('date_start') is not None:
                    result['endDate'] = formate_date_time(education.get('date_end'))
                    result['startDate'] = formate_date_time(education.get('date_start'))

                result['school'] = education.get('school')
                result['schoolAddress'] = education.get('location').get('text')
                return result
            candidate['education'] = dict(results = [])
            for education in profile.get('educations'):
                candidate['education']['results'].append(format_education(education))

        if profile.get('experiences') is not None:
            def format_experience(experience):
                result = dict()
                result['employer'] = experience.get('company')
                result['employerAddress'] = experience.get('location').get('text')
                if experience.get('date_end') is not None and experience.get('date_start') is not None:
                    result['endDate'] = formate_date_time(experience.get('date_end'))
                    result['startDate'] = formate_date_time(experience.get('date_start'))
                return result
            candidate['outsideWorkExperience'] = dict(results = [])
            for experience in profile.get('experiences'):
                candidate['outsideWorkExperience']['results'].append(format_experience(experience))

        return candidate




    def push(self, data: Dict[str, Any]):
        """
        Push profile
        Args:
            data (Dict[str, Any]): Profile
        """
        self.payload.clear()
        profile = next(data)
        self.payload.update(profile)
        response = self.send_request()
        print(response.status_code)
        if response.status_code >= 400:
            raise RuntimeError(
                "Push profile to sapsuccesfactors api-server: {} failed : `{}`".format(self.subdomain, response.content)
            )