from builtins import object
import requests
from hrflow import Hrflow
from twilio.rest import Client as Twilio


class ReceivedSMS(object):
    """
    SMS object received
    """
    def __init__(self, data: dict)-> object:
        self.ToCountry = data.get("ToCountry")  # FR
        self.ToState = data.get("ToState")  # (empty)
        self.SmsMessageSid = data.get("SmsMessageSid")  # SM5565cf41ar77c19981370121f4b7fdaf
        self.NumMedia = data.get("NumMedia")  # 0
        self.ToCity = data.get("ToCity")  # (empty)
        self.FromZip = data.get("FromZip")  # (empty)
        self.SmsSid = data.get("SmsSid")  # SM5565cf41a077c1f981370121f4b7fdaf
        self.FromState = data.get("FromState")  # (empty)
        self.SmsStatus = data.get("SmsStatus")  # received
        self.Body = data.get("Body")  # Test
        self.FromCountry = data.get("FromCountry")  # FR
        self.To = data.get("To")  # +33644650566
        self.ToZip = data.get("ToZip")  # (empty)
        self.NumSegments = data.get("NumSegments")  # 1
        self.MessageSid = data.get("MessageSid")  # SM5565cf41a077c19981370121f4b7fdaf
        self.AccountSid = data.get("AccountSid")  # ACc85f51d0657b89c8bb851ed002fa7f05
        self.From = data.get("From")  # +33646733395
        self.ApiVersion = data.get("ApiVersion")  # 2010-04-01


def workflow(body: dict, settings: dict) -> None:
    """
    CATCH WORKFLOW allows you to run a code function given an API POST request
    @rtype: null
    @param settings: dictionary of settings params of the workflow
    """
    hrflow_client = Hrflow(api_secret=settings["API_KEY"], api_user=settings["USER_EMAIL"])
    twilio_client = Twilio(settings["TWILIO_SID"], settings["TWILIO_TOKEN"])  # from twilio.com/console
    sms = ReceivedSMS(body)
    try:
        sms_parsing = hrflow_client.document.parsing.post(text=sms.Body).get("data")
    except requests.exceptions.RequestException:
        raise Exception("Parsing sms with SmsSid: %s failed " % (sms.SmsSid))
    skills = [sms.Body[ent["start"]:ent["end"]].lower() for ent in sms_parsing["ents"]
              if ent["label"] in ["HardSkill", "SoftSkill"]]
    if skills:
        message = "Here is the list of parsed skills from the SMS: "
        message += ", ".join(skills)
    else:
        message = "Sorry, no skills found! Please try another message."
    sms_sent = twilio_client.messages.create(to=sms.From, from_=sms.To, body=message)
