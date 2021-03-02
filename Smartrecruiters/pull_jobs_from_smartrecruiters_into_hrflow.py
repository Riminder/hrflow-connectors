from hrflow import Hrflow
import requests
from datetime import datetime


def hydrate(smart_job):
    return {
        "name": smart_job.get("title"),
        "agent_key": None,
        "reference": smart_job.get("refNumber"),
        "url": None,
        "created_at": smart_job.get("createdOn"),
        "updated_at": smart_job.get("updatedOn"),
        "summary": "",
        "location": {
            "text": " ".join(smart_job.get("location")[key] for key in ["country", "region", "city", "address"]
                             if smart_job.get("location").get(key)),
            "lat": smart_job.get("location").get("latitude"),
            "lng": smart_job.get("location").get("longitude")},
        "sections": [{
            "name": "company_description",
            "title": "companyDescription",
            "description": smart_job.get("jobAd").get("sections").get("companyDescription").get("text")
        },
            {
                "name": "job_description",
                "title": "jobDescription",
                "description": smart_job.get("jobAd").get("sections").get("jobDescription").get("text")
            },
            {
                "name": "qualifications",
                "title": "qualifications",
                "description": smart_job.get("jobAd").get("sections").get("qualifications").get("text")
            },
            {
                "name": "additional_information",
                "title": "additionalInformation",
                "description": smart_job.get("jobAd").get("sections").get("additionalInformation").get("text")
            }
        ],
        "skills": [],
        "languages": [{
            "name": smart_job.get("jobAd").get("language").get("label"),
            "value": None
        }],
        "tags": [{
            "name": "status",
            "value": smart_job.get("status")
        },
            {
                "name": "posting_status",
                "value": smart_job.get("postingStatus")
            },
            {
                "name": "job_uuid",
                "value": smart_job.get("id")
            },
            {
                "name": "experience_level",
                "value": smart_job.get("experienceLevel", {}).get("id")
            },
            {
                "name": "type_of_employment",
                "value": smart_job.get("typeOfEmployment", {}).get("id")
            },
            {
                "name": "industry",
                "value": smart_job.get("industry", {}).get("id")
            },
            {
                "name": "creator",
                "value": xstr(smart_job.get("creator").get("firstName")) + " " + xstr(
                    smart_job.get("creator").get("lastName"))
            },
            {
                "name": "function",
                "value": smart_job.get("function", {}).get("id")},
            {
                "name": "department",
                "value": smart_job.get("department", {}).get("id")},
            {
                "name": "manual",
                "value": smart_job.get("location", {}).get("manual")},
            {
                "name": "remote",
                "value": smart_job.get("location", {}).get("remote")},
            {
                "name": "eeo_category",
                "value": smart_job.get("eeoCategory", {}).get("id")}
        ],
        "ranges_date": [{
            "name": "targetHiringDate",
            "value_min": None,
            "value_max": smart_job.get("targetHiringDate")
        }],
        "ranges_float": [{
            "name": "compensation",
            "value_min": smart_job.get("compensation", {}).get("min"),
            "value_max": smart_job.get("compensation", {}).get("max"),
            "unit": smart_job.get("compensation", {}).get("currency")
        }],
        "metadatas": [],
    }


##### Useful functions
def format_skills(job_description, ents):
    _skills = []
    for ent in ents:
        if ent.get("label") == "HardSkill":
            skill = {
                "name": job_description[ent.get("start"):ent.get("end")],
                "value": None,
                "type": "hard"
            }
            _skills.append(skill)
        if ent.get("label") == "SoftSkill":
            skill = {
                "name": job_description[ent.get("start"):ent.get("end")],
                "value": None,
                "type": "soft"
            }
            _skills.append(skill)
    return _skills


def deduplicate_list(list_to_process):
    deduplicated_list = []
    if len(list_to_process) == 0:
        return []
    if isinstance(list_to_process[0], dict):
        for skill in list_to_process:
            if skill["name"].lower() not in [s["name"] for s in deduplicated_list]:
                deduplicated_list.append({"name": skill["name"].lower(), "value": None, "type": skill["type"]})
        return deduplicated_list
    return list(set(list_to_process))


xstr = lambda s: s or ""


def workflow(settings):
    print("HrFlow.ai client")
    api_secret = settings["API_KEY"]
    api_user = settings["USER_EMAIL"]
    hrflow_client = Hrflow(api_secret=api_secret, api_user=api_user)

    agent_key = settings["AGENT_KEY"]

    token = settings["TOKEN"]

    updated_after = settings["UPDATED_AFTER"]
    offset = _offset = int(settings["OFFSET_JOBS"])

    headers = {
        "X-SmartToken": token
    }

    params = {
        "postingStatus": "PUBLIC",
        "limit": int(settings["LIMIT"]),
        "offset": offset
    }
    if updated_after:
        params["updatedAfter"] = updated_after

    resp = requests.get(url="https://api.smartrecruiters.com/jobs", params=params, headers=headers).json()

    total_found = resp["totalFound"]

    try:
        while _offset < total_found:
            params.update({"offset": _offset})
            resp_jobs = requests.get(url="https://api.smartrecruiters.com/jobs",
                                     params=params,
                                     headers=headers).json()
            jobs = resp_jobs["content"]

            for job in jobs:
                resp_job = requests.get(url="https://api.smartrecruiters.com/jobs/" + job.get("id"),
                                        headers=headers).json()

                _job = hydrate(resp_job)
                job_hrflow = hrflow_client.job.indexing.get(board_key=settings["BOARD_KEY"],
                                                            reference=_job["reference"]).get("data")
                if job_hrflow:
                    job_hrflow["agent_key"] = agent_key
                    job_hrflow.update(_job)

                    print("Index job")
                    resp = hrflow_client.job.indexing.edit(board_key=settings["BOARD_KEY"], key=job_hrflow["key"],
                                                           job_json=job_hrflow)

                    if resp["code"] != 200:
                        raise Exception("Job is not edited")
                else:
                    _job["agent_key"] = agent_key
                    # Compute skills
                    text = "".join(xstr(section["description"]) for section in _job.get("sections"))
                    response = hrflow_client.document.parsing.post(text=text)
                    _job["skills"] = deduplicate_list(format_skills(text, response.get("data").get("ents")))
                    print("Post job")
                    post_response = hrflow_client.job.indexing.add_json(board_key=settings["BOARD_KEY"], job_json=_job)

                    if post_response["code"] != 201:
                        raise Exception("Job is not created")

                _offset += int(settings["LIMIT"])
                updated_after = datetime.utcnow().isoformat() + "Z"

    except Exception as e:
        print(e)
    settings["UPDATED_AFTER"] = updated_after
    return