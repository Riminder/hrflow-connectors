from hrflow import Hrflow
import requests
import json


def workflow(settings):
    client = Hrflow(api_secret=settings["API_KEY"], api_user=settings["USER_EMAIL"])
    profiles = client.profile.searching.list(source_keys=["source_key"], limit = 20)['data']['profiles']
    for profile in profiles:
        jobs = client.job.scoring.list(board_keys=[settings["board_key"]], source_key=settings["source_key"], profile_key=profile['key'], agent_key=settings["agent_key"], use_agent=1, limit=3, order_by="desc", sort_by="scoring")['data']['jobs']
        sendGridData = {
            "personalizations": [
                {
                    "to": [
                        {
                            "email": profile['info']['email']
                        }
                    ],
                    "dynamic_template_data": {
                        'firstname': profile['info']['first_name'],
                        'jobtitle1': jobs[0]['name'],
                        'jobdescription1': jobs[0]['summary'],
                        'joburl1' : jobs[0]['url'],
                        'jobtitle2': jobs[1]['name'], 
                        'jobdescription2': jobs[1]['summary'],
                        'joburl2' : jobs[1]['url'],
                        'jobtitle3': jobs[2]['name'], 
                        'jobdescription3': jobs[2]['summary'],
                        'joburl3' : jobs[2]['url']
                    },
                    "subject": "Top3 hot jobs for you",
                }
            ],
            "from": {
                "email": "sender_email",
                "name": "sender_mail"
            },
            "template_id": "template_id",
            "content": [
                {
                    "type": "text/html"
                }
            ],
        }
        sendGridResp = requests.post("https://api.sendgrid.com/v3/mail/send", headers={'Authorization': 'Bearer '+settings["sendgrid_api_key"],'Content-Type': 'application/json'}, data=json.dumps(sendGridData)).content
