import requests
from zipfile import ZipFile
import io
import json
from collections import namedtuple
import requests as req
from datetime import datetime
from builtins import object
from hrflow import Hrflow


class Talentsoft:

    def __init__(self, client_url, client_id, client_secret):
        self.client_url = client_url
        self.client_id = client_id
        self.client_secret = client_secret
        self.headers = self.get_headers()

    def get_headers(self):
        body = "grant_type=client_credentials&scope=MatchingIndexation&client_id=" + str(self.client_id) + "&client_secret=" + str(
            self.client_secret)
        resp = req.post(self.client_url + "api/token",
                        headers={"Content-Type": "application/x-www-form-urlencoded"},
                        data=body)

        headers = {"Authorization": "bearer " + resp.json()["access_token"]}

        return headers

    def send_report(self, payload):
        resp = req.post(self.client_url + "api/exports/v1/reports",
                        headers=self.headers,
                        json=payload)
        return resp.ok

    def pull_vacancies(self, offset, limit):
        conditions = "filter=vacancyStatus::Published"
        resp = requests.get(self.client_url + "api/exports/v1/vacancies?offset=" + str(offset) + "&limit=" + str(
            limit) + "&" + conditions, headers=self.headers, stream=True)
        reader = StreamingReader(resp.content)
        for binary_array in reader.getFiles():
            zip = ZipFile(io.BytesIO(binary_array))
            data = json.loads(str(zip.read("offerdetail"), "utf-8"),
                              object_hook=lambda d: namedtuple("X", d.keys())(*d.values()))
            yield Vacancy(zip, data)



class Vacancy(object):
    def __init__(self, zip, data):
        self.zip = zip
        self.data = data

    @property
    def status(self):
        """
        status
        """
        return self.data.offerDetail.status

    @property
    def language_id(self):
        """
        languageId
        """
        return self.data.offerDetail.languageId

    @property
    def vacancy_category(self):
        """
        vacancyCategory
        """
        return self.data.offerDetail.vacancyCategory

    @property
    def vacancy_view(self):
        """
        vacancyView
        """
        return self.data.offerDetail.vacancyView

    @property
    def candidate_view(self):
        """
        candidateView
        """
        return self.data.offerDetail.candidateView

    @property
    def organisation(self):
        """
        organisation
        """
        return self.data.offerDetail.organisation

    @property
    def candidate_redirection_url(self):
        """
        candidateRedirectionUrl
        """
        return self.data.offerDetail.candidateRedirectionUrl

    @property
    def creation_date(self):
        """
        Datetime of sending time
        """
        try:
            creation_date = self.data.offerDetail.creationDate  # iso format
        except KeyError:
            creation_date = datetime.utcnow().isoformat()
        return creation_date

    @property
    def modifiction_date(self):
        """
        Datetime of sending time
        """
        try:
            modifiction_date = self.data.offerDetail.modificationDate  # iso format
        except KeyError:
            modifiction_date = datetime.utcnow().isoformat()
        return modifiction_date

    @property
    def requesting_party(self):
        """
        requestingParty
        """
        return self.data.offerDetail.requestingParty

    @property
    def location(self):
        """
        location
        """
        text = self.data.offerDetail.location.address
        return {
            "text": text,
            "lat": None,
            "lng": None
        }

    @property
    def criteria(self):
        """
        criteria
        """
        return self.data.offerDetail.criteria

    @property
    def job_description(self):
        """
        jobDescription
        """
        return self.data.offerDetail.jobDescription

    @property
    def languages(self):
        """
        languages
        """
        _languages = []
        for language in self.data.offerDetail.languages:
            _languages.append({"name": language.language.label, "value": language.languageLevel.label})
        return _languages

    @property
    def referral(self):
        """
        referral
        """
        return self.data.offerDetail.referral

    @property
    def published_on_intranet(self):
        """
        publishedOnIntranet
        """
        return self.data.offerDetail.publishedOnIntranet

    @property
    def published_on_internet(self):
        """
        publishedOnInternet
        """
        return self.data.offerDetail.publishedOnInternet

    @property
    def reference(self):
        """
        reference
        """
        return self.data.offerDetail.reference

    @property
    def general_information_custom_fields(self):
        """
        generalInformationCustomFields
        """
        return self.data.offerDetail.generalInformationCustomFields

    @property
    def custom_block1_custom_fields(self):
        """
        customBlock1CustomFields
        """
        return self.data.offerDetail.customBlock1CustomFields

    @property
    def custom_block2_custom_fields(self):
        """
        customBlock2CustomFields
        """
        return self.data.offerDetail.customBlock2CustomFields

    @property
    def custom_block3_custom_fields(self):
        """
        customBlock3CustomFields
        """
        return self.data.offerDetail.customBlock3CustomFields

    @property
    def custom_block4_custom_fields(self):
        """
        customBlock4CustomFields
        """
        return self.data.offerDetail.customBlock4CustomFields

    @property
    def contract_type(self):
        """
        contractType
        """
        return self.data.offerDetail.jobDescription.contractType

    @property
    def sections(self):
        """
        sections
        """

        customs = self.data.offerDetail.jobDescription.jobDescriptionCustomFields

        return [{"name": "description1",
                 "title": "description1",
                 "description": self.job_description.description1 or ""
                 },
                {"name": "description2",
                 "title": "description2",
                 "description": self.job_description.description2 or ""
                 },
                {"name": "Complément du descriptif",
                 "title": "Complément du descriptif",
                 "description": "\n".join([xstr(customs.longText1), xstr(customs.longText1), xstr(customs.longText1)])
                 }]

    @property
    def tags(self):
        """
        primaryProfile=X(id="_TS_metier_profil_RH", label="Ressources humaines")
        secondaryProfiles=[], country=X(id="_TS_CO_Country_France", label="France"),
        professionalCategory=X(id="_TS_CSP_ingenieur_et_cadre", label="Ingénieur & Cadre"))


        :return:
        """
        _tags = [
            {"name": "talentsoft-organisation-id", "value": self.organisation.id},
            {"name": "talentsoft-status-id", "value": self.status.id},
            {"name": "talentsoft-professionalCategory-id", "value": self.job_description.professionalCategory.id},
            {"name": "talentsoft-country-id", "value": self.job_description.country.id},
            {"name": "talentsoft-primaryProfile-id", "value": self.job_description.primaryProfile.id},
            {"name": "talentsoft-contractType-id", "value": self.contract_type.id},
            {"name": "talentsoft-publishedOnInternet", "value": self.published_on_internet},
            {"name": "talentsoft-publishedOnIntranet", "value": self.published_on_intranet}
        ]
        if self.criteria.experienceLevel:
            _tags.append({"name": "talentsoft-experienceLevel", "value": self.criteria.experienceLevel.id})

        if self.criteria.educationLevel:
            _tags.append({"name": "talentsoft-educationLevel", "value": self.criteria.educationLevel.id})

        return _tags

    @property
    def item(self):
        data = {
            "name": self.job_description.title,
            "reference": self.reference,
            "url": None,
            "summary": "",
            "location": self.location,
            "sections": self.sections,
            "ranges_float": [],
            "ranges_date": [],
            "skills": self.criteria.skills,
            "languages": self.languages,
            "labels": [],
            "tags": self.tags,
            "metadatas": [],
            "created_at": self.creation_date
        }
        return data

    def callback(self, status, message=None):
        # send reports to Talentsoft
        item = {
            "id": self.item.get("reference"),
            "date": self.item.get("created_at"),
            "lastUpdateDate": datetime.utcnow().isoformat(),
            "status": status,
        }
        if message:
            item["messages"] = [message]

        report = {
            "reportType": "Vacancies",
            "items": [item]
        }
        return report


class StreamingReader:
    def __init__(self, content):
        self._readingDataBlock = False
        self._content = content
        self._file = None
        self._resetChunk()
        self._resetDataBlock()

    def _resetDataBlock(self):
        self._currentZip = b""
        self._currentZipSize = 0

    def _resetChunk(self):
        self._currentChunk = None
        self._indexChunk = 0

    def getFiles(self):
        for self._currentChunk in [self._content]:
            while len(self._currentChunk) > self._indexChunk:
                if self._readingDataBlock:
                    self._readDataBlock()
                    if self._file is not None:
                        yield self._file
                        self._file = None
                else:
                    self._readSizeBlock()

    def _readDataBlock(self):
        sizeToRead = self._currentZipSize
        self._currentZip = self._currentChunk[self._indexChunk:self._indexChunk + sizeToRead]
        self._indexChunk += sizeToRead
        if True or len(self._currentZip) == self._currentZipSize:
            self._readingDataBlock = False
            self._file = self._currentZip
            self._resetDataBlock()

    def _readSizeBlock(self):
        self._currentZipSize = int.from_bytes(self._currentChunk[self._indexChunk:self._indexChunk + 4],
                                              byteorder="little")
        self._indexChunk += 4
        self._readingDataBlock = True

def format_skills(text: str, ents: list) -> list:
    """
    Get the list of skills according to the HrFlow.ai Job format
    @param text: text description of the job
    @param ents: list of entities in the text
    @return: list of skills
    """
    skills = [{ "name": text[ent["start"]:ent["end"]].lower(), "value": None,
                "type": "hard" if ent["label"] == "HardSkill" else "soft"}
              for ent in ents if ent["label"].endswith("Skill")]
    return list({v["name"]:v for v in skills}.values())

xstr = lambda s: s or ""

def workflow(settings: dict) -> None:
    """
    WORKFLOW to pull profiles
    @rtype: null
    @param settings: dictionary of settings params of the workflow
    """
    hrflow_client = Hrflow(api_secret=settings["API_KEY"], api_user=settings["USER_EMAIL"])

    talentsoft_client = Talentsoft(settings["BASE_URL"], settings["CLIENT_ID"], settings["CLIENT_SECRET"])

    hrflow_job = talentsoft_client.pull_vacancies(settings["OFFSET"], settings["LIMIT"])

    for job in hrflow_job:
       job_json = job.item
       verify_job = hrflow_client.job.indexing.get(board_key=settings["BOARD_KEY"], reference=job_json["reference"]).get("data")
       if not verify_job:
           try:
               job_json["agent_key"] = settings["AGENT_KEY"]
               # Parse skills
               SECTION_SEPARATOR = "\n\n"  # important to separate sections by double line jumps
               job_text = SECTION_SEPARATOR.join(section["description"] or "" for section in job_json["sections"])
               job_parsing = hrflow_client.document.parsing.post(text=job_text).get("data")
               job_json["skills"] = format_skills(job_text, job_parsing["ents"])
               # Save Job
               resp = hrflow_client.job.indexing.add_json(board_key=settings["BOARD_KEY"], job_json=job_json)

               if resp["code"] == 201:
                   print("Report sent for profile %s " % (job_json.get("reference")))
                   talentsoft_client.send_report(job.callback("success"))
           except requests.exceptions.RequestException:
               raise Exception("Saving job with reference %s failed" % (job_json["reference"]))
