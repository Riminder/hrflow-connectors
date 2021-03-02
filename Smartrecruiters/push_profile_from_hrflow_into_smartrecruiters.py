from mimetypes import MimeTypes
import base64
from hrflow import Hrflow
import requests

def format_educations(educations):
    _educations = []

    for edu in educations:
        _edu = format_project(edu)
        _edu.update({
            "institution": edu.get("school") if edu.get("school") else "NaN",
            "degree": edu.get("title") if edu.get("title") else "NaN",
            "major": "NaN"
        })

        _educations.append(_edu)

    return _educations


def format_experiences(experiences):
    _experiences = []

    for exp in experiences:
        _exp = format_project(exp)
        _exp.update({
            "title": exp.get("title") if exp.get("title") else "NaN",
            "company": exp.get("company") if exp.get("company") else "NaN"
        })

        _experiences.append(_exp)

    return _experiences


def format_project(project):
    return {
        "current": False,
        "startDate": project.get("date_start").split("T")[0] if project.get("date_start") else "1980-01-01",
        "endDate": project.get("date_end").split("T")[0] if project.get("date_end") else "2100-01-01",
        "location": xstr(project.get("location").get("text")),
        "description": project.get("description")
    }


def format_resume(profile):
    _attachment = profile.get("attachments")
    mime = MimeTypes()
    for attachment in profile.get("attachments"):
        if attachment.get("type") == "resume":
            content = base64.b64encode(requests.get(attachment["public_url"]).content).decode("utf-8")
            mime_type = mime.guess_type(attachment["public_url"])[0]
            return {
                "fileName": attachment.get("file_name") + attachment.get("extension"),
                "mimeType": mime_type,
                "fileContent": content
            }


def hydrate(profile):
    info = profile.get("info")
    return {
        "firstName": info.get("first_name"),
        "lastName": info.get("last_name"),
        "email": info.get("email"),
        "phoneNumber": info.get("phone"),
        "location": {
            "country": xstr(info.get("location").get("fields").get("country")),
            "countryCode": "No",
            "region": xstr(info.get("location").get("fields").get("state")),
            "regionCode": "NaN",
            "city": xstr(info.get("location").get("fields").get("city")),
            "lat": info.get("location").get("lat") if info.get("location").get("lat") else 64.7805,
            "lng": info.get("location").get("lng") if info.get("location").get("lng") else -147.3694
        },
        "web": {
            "skype": "",
            "linkedIn": xstr(info.get("location").get("linkedin")),
            "facebook": xstr(info.get("location").get("facebook")),
            "twitter": xstr(info.get("location").get("twitter")),
            "website": ""
        },
        "tags": [],
        "education": format_educations(profile.get("educations")),
        "experience": format_experiences(profile.get("experiences")),
        "attachments": [],
        "consent": True
    }


def get_tag_value(tags, name):
    for tag in tags:
        if tag["name"] == name:
            return tag.get("value")
    return None


xstr = lambda s: s or " "


def workflow(body, settings):
    print("Welcome to catch: new application")

    profile_key = body["profile_key"]
    source_key = body["source_key"]

    job_key = body["job_key"]
    board_key = body["board_key"]

    token = settings["TOKEN"]

    headers = {
        "X-SmartToken": token
    }

    client = Hrflow(api_secret=settings["API_KEY"], api_user=settings["USER_EMAIL"])

    job = client.job.indexing.get(board_key=board_key, key=job_key).get("data")

    job_uuid = get_tag_value(job.get("tags"), "job_uuid")

    profile = client.profile.indexing.get(source_key=source_key, key=profile_key).get("data")

    smart_candidate = hydrate(profile)
    url = "https://api.smartrecruiters.com/jobs/" + job_uuid + "/candidates"
    smart_response = requests.post(url=url, json=smart_candidate, headers=headers)

    if not smart_response.ok:
        raise Exception("Invalid response from smartrecruiters API")

    profile_uuid = smart_response.json().get("id")

    url_attachment = "https://api.smartrecruiters.com/candidates/" + profile_uuid + "/jobs/" + job_uuid + "/attachments/"
    add_attachment_response = requests.post(url=url_attachment,
                                            headers=headers,
                                            data={
                                                "attachmentType": "RESUME"
                                            },
                                            files={"file": format_resume(profile).get("fileContent")})
    if not add_attachment_response.ok:
        raise Exception("Invalid response while adding Resume")

