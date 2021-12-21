from hrflow import Hrflow
import mailchimp_transactional as MailchimpTransactional
from mailchimp_transactional.api_client import ApiClientError


def get_profile_event(_request: dict) -> dict:
    """
    Reconstruct webhook body
    @param _request: POST request
    @return: webhook event
    """
    return {
        "type": _request.get("type"),
        "origin": _request.get("type"),
        "message": _request.get("message"),
        "profile": _request.get("profile")
    }


def verify_webhook(signature: str, event: dict, secret: str) -> null:
    """
    Verify Webhook Signature
    @param signature: webhook integrity signature
    @param event: webhook data event
    @param secret: webhook secret key
    """
    import json
    import hmac
    import hashlib
    message = json.dumps(event, separators=(",", ":")).encode()
    hasher = hmac.new(secret.encode(), message, hashlib.sha256)
    dig = hasher.hexdigest()
    assert hmac.compare_digest(dig, signature)


def workflow(_request: dict, settings: dict) -> None:
    """
    WORKFLOW to send an email to a profile after catching a wehook parsing success event
    @rtype: null
    @param _request: dictionary that contains the body and the headers of the request
    @param settings: dictionary of settings params of the workflow
    """
    if not _request.get("profile"):
        return
    webhook_event = get_profile_event(_request)
    assert webhook_event["type"] == "profile.parsing.success"  # This workflow is only to create a new profile
    verify_webhook(_request["HTTP-HRFLOW-SIGNATURE"], webhook_event, settings["HRFLOW_WEBHOOK_SECRET"])
    profile_key = webhook_event["profile"]["key"]
    source_key = webhook_event["profile"]["source"]["key"]
    # Retrieve Profile from HrFlow.ai
    hrflow_client = Hrflow(api_secret=settings["HRFLOW_API_SECRET"], api_user=settings["HRFLOW_API_USER"])
    profile = hrflow_client.profile.indexing.get(source_key=source_key, key=profile_key).get("data")
    assert profile is not None
    # send Email with Mailchimp
    mailchimp = MailchimpTransactional.Client(settings["MANDRILL_API_TOKEN"])
    message = {
        "from_email": settings["HRFLOW_API_USER"],
        "subject": "Thank you for your application",
        "text": "Your application is well received. Our team will reach out to you if there are any opportunities "
                "matching your profile. Best, the Team",
        "to": [
            {
                "email": profile["info"]["email"],
                "type": "to"
            }
        ]
    }
    try:
        response = mailchimp.messages.send({"message": message})
        print("AcPI called successfully: {}".format(response))
    except ApiClientError as error:
        print("An excception occurred: {}".format(error.text))
