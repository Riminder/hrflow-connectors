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


def format_location(location: dict) -> dict:
    return {
        "city": location["fields"]["city"] if location["fields"] else None,
        "house_number": location["fields"]["house_number"] if location["fields"] else None,
        "district": location["fields"]["city_district"] if location["fields"] else None,
        "postcode": location["fields"]["postcode"] if location["fields"] else None,
        "lng": location["lng"],
        "lat": location["lat"],
        "Country": location["fields"]["country"] if location["fields"] else None
    }


def format_education(education: dict) -> dict:
    _education = format_project(education)
    _education["content_uid"] = education["key"]
    _education["school"] = education["school"] if education["school"] else "NaN"
    _education["eduLocation"] = format_location(education["location"])
    return _education


def format_experience(experience: dict) -> dict:
    _experience = format_project(experience)
    _experience["experience_key"] = experience["key"]
    _experience["company"] = experience["company"]
    _experience["expLocation"] = format_location(experience["location"])
    return _experience


def format_project(project: dict) -> dict:
    return {
        "title": project["title"] if project["title"] else "NaN",
        "description": project["description"],
        "startDate": project["date_start"].split("T")[0] if project["date_start"] else None,
        "endDate": project["date_end"].split("T")[0] if project["date_end"] else None,
        "hard_skills": project["hard_skills"],
        "soft_skills": project["soft_skills"]
    }


def format_attachment(attachment: dict) -> dict:
    return {
        "alt": attachment["alt"],
        "attachment_key": "",
        "extension": attachment["extension"],
        "file_name": attachment["file_name"],
        "file_size": attachment["file_size"],
        "original_file_name": attachment["original_file_name"],
        "public_url": attachment["public_url"],
        "type": attachment["type"]
    }


def format_skill(skill: dict, item_key: str) -> dict:
    return {
        "skill_type": skill["type"],
        "skill_name": skill["name"],
        "skill_value": skill["value"],
        "ContactId": item_key
    }


def format_object(obj: dict, obj_name: str, item_key: str) -> dict:
    return {
        obj_name + "_name": obj["name"],
        obj_name + "_value": obj["value"],
        obj_name + "_key": obj["name"].lower(),
        "ContactId": item_key
    }


def format_label(label: dict, item_key: str) -> dict:
    return {
        "label_key": "",
        "job_key": label["job_key"],
        "stage": label["stage"],
        "ContactId": item_key
    }


def format_crosstalent_profile(profile: dict) -> dict:
    return {
        "myMaster": {
            "profile_id": profile["id"],
            "profile_key": profile["key"],
            "profile_reference": profile["reference"],
            "educations_duration": str(round(float(profile["educations_duration"]))),
            "experiences_duration": str(round(float(profile["experiences_duration"]))),
            "profile_description": profile["text"],
            "profile_language": profile["text_language"],
            "archive": True if profile["archive"] else False,
            "myConsent": {
                "ownerEmbedding": profile["consent_algorithmic"]["owner"]["embedding"],
                "ownerParsing": profile["consent_algorithmic"]["owner"]["parsing"],
                "ownerReasoning": profile["consent_algorithmic"]["owner"]["reasoning"],
                "ownerRevealing": profile["consent_algorithmic"]["owner"]["revealing"],
                "ownerScoring": profile["consent_algorithmic"]["owner"]["scoring"],
                "ownerSearching": profile["consent_algorithmic"]["owner"]["searching"],
                "ControllerEmbedding": profile["consent_algorithmic"]["controller"]["embedding"],
                "ControllerParsing": profile["consent_algorithmic"]["controller"]["parsing"],
                "ControllerReasoning": profile["consent_algorithmic"]["controller"]["reasoning"],
                "ControllerRevealing": profile["consent_algorithmic"]["controller"]["revealing"],
                "ControllerScoring": profile["consent_algorithmic"]["controller"]["scoring"],
                "ControllerSearching": profile["consent_algorithmic"]["controller"]["searching"],
                "consent_key": None,  # TODO
                "contactId": profile["key"]  # TODO
            },
            "mySource": {
                "type": profile["source"]["type"],
                "subtype": profile["source"]["subtype"],
                "source_name": profile["source"]["name"],
                "source_key": profile["source"]["key"]
            },
            "myInfo": {
                "first_name": profile["info"]["first_name"],
                "last_name": profile["info"]["last_name"],
                "gender": profile["info"]["gender"],
                "phone": profile["info"]["phone"],
                "summary": profile["info"]["summary"],
                "email": profile["info"]["email"],
                "contactAddress": format_location(profile["info"]["location"]),
                "socialMediaUrls": {
                    "facebook_url":
                        next((item for item in profile["info"]["urls"] if item["type"] == "facebook"), None)["url"],
                    "twitter_url":
                        next((item for item in profile["info"]["urls"] if item["type"] == "twitter"), None)["url"],
                    "github_url":
                        next((item for item in profile["info"]["urls"] if item["type"] == "github"), None)["url"],
                    "linkedin_url":
                        next((item for item in profile["info"]["urls"] if item["type"] == "linkedin"), None)["url"],
                    "socialMedial_from_resume":
                        next((item for item in profile["info"]["urls"] if item["type"] == "from_resume"), None)["url"]
                }

            },
            "myEducation": [format_education(edu) for edu in profile["educations"]],
            "myExperience": [format_experience(exp) for exp in profile["experiences"]],
            "myAttachments": [format_attachment(attachment) for attachment in profile["attachments"]],
            "mySkills": [format_skill(skill, profile["key"]) for skill in profile["skills"]],
            "myLanguages": [format_object(language, "language", profile["key"]) for language in profile["languages"]],
            "myInterests": [format_object(interest, "interest", profile["key"]) for interest in profile["interests"]],
            "myLabels": [format_label(label, profile["key"]) for label in profile["labels"]],
            "myTags": [format_object(tag, "tag", profile["key"]) for tag in profile["tags"]],
            "myMetadata": [format_object(metadata, "metadata", profile["key"]) for metadata in profile["tags"]]
        }
    }


def workflow(body: dict, settings: dict) -> None:
    """
    CATCH WORKFLOW allows you to run a code function given an API POST request
    @rtype: None
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
        raise Exception('Retrieving token from Crosstalent failed')
    # Get HrFlow.ai Profile
    try:
        profile_hrflow = hrflow_client.profile.indexing.get(source_key=event.source_key, key=event.profile_key).get(
            'data')
    except requests.exceptions.RequestException:
        raise Exception('Retrieving profile with profile_key: %s and source_key: %s failed' % (event.profile_key, event.source_key))
    # Create HrFlow.ai Profile in Crosstalent
    # TODO: verify that the profile doesn't exist yet in Crosstalent
    try:
        headers = {
            "Authorization": "OAuth " + crosstalent_token,
            "Content-Type": "application/json"
        }
        crosstalent_profile_endpoint = "https://{}.salesforce.com/services/apexrest/HrFlowCreateProfile/".format(
            settings["CT_URL"])
        r = requests.post(crosstalent_profile_endpoint,
                          json=format_crosstalent_profile(profile_hrflow),
                          headers=headers)
    except requests.exceptions.RequestException:
        raise Exception('Saving profile with profile_key: %s and source_key: %s failed' % (event.profile_key, event.source_key))