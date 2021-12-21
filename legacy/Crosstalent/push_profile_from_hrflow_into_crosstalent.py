from builtins import object
from hrflow import Hrflow
import requests

# TODO: verify webhook signature
"""
import hmac
import hashlib
def check_signature(request_signature, secret_key, request_body):
    hasher = hmac.new(secret_key, request_body, hashlib.sha256)
    dig = hasher.hexdigest()
    return hmac.compare_digest(dig, request_signature)
"""


class WebhookEvent(object):
    """
    CATCH Webhook Event
    #TODO signature verification
    """

    def __init__(self, data):
        self.type = data.get("type")
        self.origin = data.get("origin")
        self.message = data.get("message")
        self.profile = data.get("profile")
        if self.profile:
            self.profile_key = self.profile["key"]
            self.source_key = self.profile["source"]["key"]


def workflow(body: dict, settings: dict) -> None:
    """
    CATCH WORKFLOW allows you to run a code function given an API POST request
    @rtype: null
    @param body: POST request Body
    @param settings: dictionary of settings params of the workflow
    """
    event = WebhookEvent(body)
    if not event.profile:
        return
    assert event.type == "profile.parsing.success"  # This workflow is only to create a new profile
    hrflow_client = Hrflow(api_secret=settings["API_KEY"], api_user=settings["USER_EMAIL"])
    crosstalent_api_url = "https://{}.salesforce.com/services/oauth2/token".format(settings["CT_ENV"])
    # Get Crosstalent Token
    try:
        payload = {
            "grant_type": "password",
            "client_id": settings["CT_CLIENT_ID"],
            "client_secret": settings["CT_CLIENT_SECRET"],
            "username": settings["CT_USERNAME"],
            "password": settings["CT_PASSWORD"]
        }
        crosstalent_token = requests.post(crosstalent_api_url, data=payload).json()["access_token"]
    except requests.exceptions.RequestException:
        raise Exception("Retrieving token from Crosstalent failed")
    # Get HrFlow.ai Profile
    try:
        profile_hrflow = hrflow_client.profile.indexing.get(source_key=event.source_key, key=event.profile_key).get(
            "data")
    except requests.exceptions.RequestException:
        raise Exception("Retrieving profile with profile_key: %s and source_key: %s failed" % (event.profile_key, event.source_key))
    # Create HrFlow.ai Profile in Crosstalent
    # TODO: verify that the profile doesn"t exist yet in Crosstalent
    try:
        headers = {
            "Authorization": "OAuth " + crosstalent_token,
            "Content-Type": "application/json"
        }
        crosstalent_profile_endpoint = "https://{}.salesforce.com/services/apexrest/HrFlowCreateProfile/".format(
            settings["CT_URL"])
        r = requests.post(crosstalent_profile_endpoint,
                          json=profile_hrflow,
                          headers=headers)
    except requests.exceptions.RequestException:
        raise Exception("Saving profile with profile_key: %s and source_key: %s failed" % (event.profile_key, event.source_key))