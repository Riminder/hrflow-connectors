# Code from PR: https://github.com/Riminder/hrflow-connectors/pull/66
# Author: Yasser BELMEHDI (yass1337)
import json
import typing as t
from datetime import datetime

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.taleez.warehouse import (
    TaleezJobWarehouse,
    TaleezProfilesWarehouse,
)
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)
from hrflow_connectors.core.warehouse import ActionEndpoints

# TODO: These are examples custom properties
# --> Should be added as parameters to configure for each customer
PROPERTIES_PATH = "src/hrflow_connectors/connectors/taleez/properties.json"
DATE_FORMAT = "%Y-%m-%d"
INITIALREF = "HrFlow"
HRFLOW_TAGS_MAPPING = {
    "experience": 51933,
    "fr_education_level": 51934,
    "availability": 51935,
    "salary_wanted": 51936,
    "candidate_profile": 51937,
    "fr_contract": 51938,
    "CV": 51939,
    "source_candidate": 51940,
    "en_education_level": 56258,
    "candidate_contract": 56259,
    "last_position": 56263,
    "hard_skills": 56260,
    "languages": 56261,
    "courses": 56262,
    "last_education": 59898,
}

DEFAULT_CANDIDATE_SOURCE = "HrFlow"

EDUCATION_LEVELS_MAPPING = {
    600944: "Aucun diplôme",
    600945: "BEP, CAP ou équivalent",
    600946: "Baccalauréat (bac +0)",
    600947: "DUT, BTS, Diplôme d'état ou équivalent (bac +2)",
    600948: "Licence, Bachelor ou équivalent (bac +3)",
    600949: "Master 1 ou équivalent (bac +4)",
    600950: "Master, Diplôme d'ingénieur ou équivalent (bac +5)",
    600951: "Doctorat et équivalent (> bac +5)",
}

SOCIAL_LINKS = [
    "linkedin",
    "viadeo",
    "twitter",
    "github",
    "behance",
    "other",
    "website",
    "dribble",
]

TALEEZ_JOBS_ENDPOINT = "https://api.taleez.com/0/jobs"
TALEEZ_JOBS_ENDPOINT_LIMIT = 100


GET_ALL_JOBS_ENDPOINT = ActionEndpoints(
    name="Get all jobs",
    description=(
        "Endpoint to retrieve all jobs."
        " and get the list of all jobs with their ids, the request method"
        " is `GET`"
    ),
    url="https://api.taleez.com/0/jobs",
)


def get_education_level(profile: t.Dict):
    fr_education_level = []
    for education in profile["educations"]:
        for id in EDUCATION_LEVELS_MAPPING.keys():
            for keyword in EDUCATION_LEVELS_MAPPING[id]:
                if keyword in education["title"]:
                    fr_education_level.append(id)
    return fr_education_level


def get_CV(profile: t.Dict):
    return next(
        filter(lambda x: x.get("type") == "resume", profile["attachments"]), {}
    ).get("public_url")


def get_property_by_id(
    properties_list: t.List[t.Dict], prop_id: int
) -> t.Optional[t.Dict]:
    return next(iter(filter(lambda x: x["id"] == prop_id, properties_list)), None)


def tags_to_properties_mapping(properties: t.List[t.Dict]) -> t.Dict:
    mapping = dict()
    for tag_name, id_ in HRFLOW_TAGS_MAPPING.items():
        p = get_property_by_id(properties, id_)
        mapping[tag_name] = {"id": id_, "key": p["key"], "type": p["type"]}

    return mapping


class Property:
    def __init__(
        self,
        id: int,
        key: str,
        name: str,
        nameEn: str,
        type: str,
        choices: t.List[t.Dict],
    ) -> None:
        self.id = id
        self.key = key
        self.name = name
        self.nameEn = nameEn
        self.type = type
        self.choices = choices

    def get_property_choice_from_text(
        self, input_text: str, processor: t.Callable[[str], t.List[int]]
    ) -> t.List[int]:
        return processor(input_text)

    def generate_property_payload(
        self,
        input_text: t.Union[str, int],
        processor: t.Optional[t.Callable[[str], t.List[int]]] = None,
    ) -> t.Dict:
        payload = None
        if self.type in ["INPUT", "TEXTAREA", "DATE"]:
            payload = {"id": self.id, "value": input_text}
        elif self.type == "SELECT":
            assert processor is not None, (
                "for SELECT type Property a processor function to select choice id must"
                " be provided"
            )
            choices = self.get_property_choice_from_text(input_text, processor)
            assert len(choices) <= 1, (
                "for SELECT type Property, at most one choice must be selected. check"
                " processor function."
            )
            payload = {"id": self.id, "choices": choices}
        elif self.type == "SELECTMUL":
            assert processor is not None, (
                "for SELECTMUL type Property a processor function to select choice id"
                " must be provided"
            )
            choices = self.get_property_choice_from_text(input_text, processor)
            payload = {"id": self.id, "choices": choices}
        return payload


def get_parsed_hardskills(profile: t.Dict):
    skills = list(filter(lambda x: x["type"] == "hard", profile["skills"]))
    skills = [skill["name"] for skill in skills] if skills else []
    return ", ".join(skills) if skills else ""


def get_languages(profile: t.Dict):
    return (
        ", ".join([language["name"] for language in profile["languages"]])
        if profile["languages"]
        else ""
    )


def get_courses(profile: t.Dict):
    return (
        ", ".join([course["name"] for course in profile["courses"]])
        if profile["courses"]
        else ""
    )


def get_last_position(profile: t.Dict):
    return profile["experiences"][0]["title"]


def get_hrflow_tag_by_name(hrflow_profile: t.Dict, tag_name: str) -> t.Optional[t.Dict]:
    return next(filter(lambda x: x["name"] == tag_name, hrflow_profile["tags"]), {})


def get_profile_properties_to_push(
    profile: t.Dict, origin_properties: t.List[t.Dict]
) -> t.List[t.Dict]:
    """Takes hrflow profile object and the list of all properties
        from Taleez candidates endpoint
        Generates Property objects to leverage class methods
        to format tag values and each tag specific
        processor to format the text if needed or
        select the corresponding choice from choices list in case
        of a select field depending on the rule fixed in the processor function.
    Args:
        profile (t.Dict): HrFlow.ai Profile
        origin_properties (t.List[t.Dict]): List of properties
        created by the customer on Taleez (result of get candidate-properties)
    Returns:
        t.List[t.Dict]: List of properties payload
        to post via candidate-properties endpoint
    """
    output = []
    property_objects = dict()
    for tag_name, property_id in HRFLOW_TAGS_MAPPING.items():
        property_dict = get_property_by_id(origin_properties, property_id)
        if property_dict:
            property_objects[tag_name] = Property(**property_dict)

    tag_name = "source_candidate"
    input_text = profile["source"]["name"]
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )

    tag_name = "hard_skills"
    input_text = get_parsed_hardskills(profile)
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )

    tag_name = "languages"
    input_text = get_languages(profile)
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )

    tag_name = "courses"
    input_text = get_courses(profile)
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )

    tag_name = "last_position"
    input_text = profile["experiences"][0]["title"] if profile["experiences"] else ""
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )

    tag_name = "last_education"
    input_text = profile["educations"][0]["title"] if profile["educations"] else ""
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )

    return output


def format_profile(profile: t.Dict) -> t.Dict:
    social_links = {}
    for link in profile["info"]["urls"]:
        if link["type"] in SOCIAL_LINKS:
            social_links["{}".format(link["type"])] = link["url"]
    candidate = dict(
        firstName=profile["info"]["first_name"],
        lastName=profile["info"]["last_name"],
        mail=profile["info"]["email"],
        initialReferrer=INITIALREF,
        lang=str(profile["text_language"]).upper(),
        social_links=social_links,
    )
    cv = get_CV(profile)
    with open(PROPERTIES_PATH) as f:
        properties = get_profile_properties_to_push(profile, json.load(f)["list"])
    return dict(candidate=candidate, CV=cv, properties=properties)


def get_job_location(taleez_job: t.Union[t.Dict, None]) -> t.Dict:
    if taleez_job is None:
        return dict(lat=None, lng=None, text="")

    lat = taleez_job.get("lat")
    lat = float(lat) if lat is not None else lat

    lng = taleez_job.get("lng")
    lng = float(lng) if lng is not None else lng

    concatenate = []
    for field in ["postalCode", "city", "country"]:
        if taleez_job.get(field):
            concatenate.append(taleez_job.get(field))

    return dict(lat=lat, lng=lng, text=" ".join(concatenate))


def get_sections(taleez_job: t.Dict) -> t.List[t.Dict]:
    sections = []
    for section_name in ["jobDescription", "profileDescription", "companyDescription"]:
        section = taleez_job.get(section_name)
        if section is not None:
            sections.append(
                dict(
                    name="taleez-sections-{}".format(section_name),
                    title=section_name,
                    description=section,
                )
            )
    return sections


def get_tags(taleez_job: t.Dict) -> t.List[t.Dict]:
    taleez_tags = taleez_job.get("tags")
    if taleez_tags is None:
        return []
    t = lambda name, value: dict(name=name, value=value)

    custom_fields = [
        "contract",
        "profile",
        "urlApplying",
        "currentStatus",
    ]

    tags = [
        t("{}_{}".format("taleez", field), taleez_job.get(field))
        for field in custom_fields
    ]
    tags += [t("{}_{}".format("taleez", "tag"), tag) for tag in taleez_tags]
    return tags


def format_job(taleez_job: t.Dict) -> t.Dict:
    job = dict(
        name=taleez_job.get("label", "Undefined"),
        reference=str(taleez_job.get("id")),
        created_at=datetime.fromtimestamp(taleez_job["dateCreation"]).isoformat(),
        updated_at=datetime.fromtimestamp(taleez_job["dateLastPublish"]).isoformat(),
        location=get_job_location(taleez_job),
        url=taleez_job.get("url"),
        summary=None,
        sections=get_sections(taleez_job),
        tags=get_tags(taleez_job),
    )
    return job


DESCRIPTION = (
    "Taleez est une solution globale de gestion des candidatures et de diffusion"
    " d'offres d'emploi.Pilotez intégralement vos processus de recrutement et intégrez"
    " vos équipes dans les décisions."
)

Taleez = Connector(
    name="Taleez",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://taleez.com/",
    actions=[
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Retrieves a profile from HrFlow Source and posts it to Taleez ATS "
                "enriching it with properties extracted from the profile"
            ),
            parameters=BaseActionParameters.with_defaults(
                # FIXME : why is it called ReadJobsActionParameters ?
                "ReadJobsActionParameters",
                format=format_profile,
            ),
            origin=HrFlowProfileWarehouse,
            target=TaleezProfilesWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs via the ***Taleez*** API and send them"
                " to a ***Hrflow.ai Board***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "PullJobsActionParameters", format=format_job
            ),
            origin=TaleezJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
