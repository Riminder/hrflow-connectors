# Code from PR: https://github.com/Riminder/hrflow-connectors/pull/66
# Author: Yasser BELMEHDI (yass1337)
# import json
import os
import typing as t
from datetime import datetime

from hrflow_connectors.v2.connectors.taleez.warehouse import TaleezWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow

# TODO: These are examples custom properties
# --> Should be added as parameters to configure for each customer
PROPERTIES_PATH = os.path.join(os.path.dirname(__file__), "properties.json")
DATE_FORMAT = "%Y-%m-%d"
INITIALREF = "HrFlow"
HRFLOW_TAGS_MAPPING = {
    "experience": 90340,
    "fr_education_level": 90341,
    "availability": 90342,
    "salary_wanted": 90343,
    "candidate_profile": 90344,
    "fr_contract": 90345,
    "CV": 90346,
    "source_candidate": 90347,
    # "en_education_level": 90339,
    # "candidate_contract": 90338,
    # "last_position": 90334,
    # "hard_skills": 90337,
    # "languages": 90336,
    # "courses": 90335,
    # "last_education": 90333,
}

DEFAULT_CANDIDATE_SOURCE = "HrFlow"

EDUCATION_LEVELS_MAPPING = {
    1061171: "Aucun diplôme",
    1061172: "BEP, CAP ou équivalent",
    1061173: "Baccalauréat (bac +0)",
    1061174: "DUT, BTS, Diplôme d'état ou équivalent (bac +2)",
    1061175: "Licence, Bachelor ou équivalent (bac +3)",
    1061176: "Master 1 ou équivalent (bac +4)",
    1061177: "Master, Diplôme d'ingénieur ou équivalent (bac +5)",
    1061178: "Doctorat et équivalent (> bac +5)",
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


def get_education_level(profile: t.Dict):
    fr_education_level = []
    for education in profile["educations"]:
        for id in EDUCATION_LEVELS_MAPPING.keys():
            for keyword in EDUCATION_LEVELS_MAPPING[id]:
                if keyword in education["title"]:
                    fr_education_level.append(id)
    return fr_education_level


def get_property_by_id(
    properties_list: t.List[t.Dict], prop_id: int
) -> t.Optional[t.Dict]:
    return next(iter(filter(lambda x: x["id"] == prop_id, properties_list)), None)


def tags_to_properties_mapping(properties: t.List[t.Dict]) -> t.Dict:
    mapping = dict()
    for tag_name, id_ in HRFLOW_TAGS_MAPPING.items():
        p = get_property_by_id(properties, id_)
        if p:
            mapping[tag_name] = {"id": id_, "key": p["key"], "type": p["type"]}
        else:
            mapping[tag_name] = None
    return mapping


class Property:
    def __init__(
        self,
        id: int,
        internal: str,
        apiKey: str,
        name: str,
        nameEn: str,
        type: str,
        disabled: bool,
        choices: t.List[t.Dict],
    ) -> None:
        self.id = id
        self.internal = internal
        self.apiKey = apiKey
        self.name = name
        self.nameEn = nameEn
        self.type = type
        self.disabled = disabled
        self.choices = choices

    def get_property_choice_from_text(
        self, input_text: str, processor: t.Callable[[str], t.List[int]]
    ) -> t.List[int]:
        return processor(input_text)

    def generate_property_payload(
        self,
        input_text: str,
        processor: t.Optional[t.Callable[[str], t.List[int]]] = None,
    ) -> t.Optional[t.Dict]:
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


def format_hrflow_profile(hrflow_profile: t.Dict) -> t.Dict:
    social_links = {}
    for link in hrflow_profile["info"]["urls"]:
        if link["type"] in SOCIAL_LINKS:
            social_links["{}".format(link["type"])] = link["url"]
    candidate = dict(
        firstName=hrflow_profile["info"]["first_name"],
        lastName=hrflow_profile["info"]["last_name"],
        mail=hrflow_profile["info"]["email"],
        phone=hrflow_profile["info"]["phone"],
        initialReferrer=INITIALREF,
        lang=str(hrflow_profile["text_language"]).upper(),
        social_links=social_links,
        location=dict(
            country=hrflow_profile["info"]["location"]
            .get("fields", {})
            .get("country")[:2]
            .upper(),
            lat=hrflow_profile["info"]["location"]["lat"],
            lng=hrflow_profile["info"]["location"]["lng"],
            city=hrflow_profile["info"]["location"].get("fields", {}).get("city"),
            postalCode=hrflow_profile["info"]["location"]
            .get("fields", {})
            .get("postcode"),
            street=hrflow_profile["info"]["location"].get("fields", {}).get("road"),
            streetNumber=hrflow_profile["info"]["location"]
            .get("fields", {})
            .get("house_number"),
        ),
    )
    resume = next(
        filter(lambda x: x.get("type") == "resume", hrflow_profile["attachments"]), {}
    ).get("public_url")

    # TODO: Add properties to the candidate object
    # with open(PROPERTIES_PATH) as f:
    #     properties = get_profile_properties_to_push(
    #         hrflow_profile, json.load(f)["list"]
    #     )

    return dict(
        candidate=candidate,
        resume=resume,
    )


def format_hrflow_profile_for_update(hrflow_profile: t.Dict) -> t.Dict:
    profile_item = format_hrflow_profile(hrflow_profile)
    return dict(
        id=hrflow_profile["reference"],
        candidate=profile_item["candidate"],
    )


def get_location(taleez_object: t.Union[t.Dict, None]) -> t.Dict:
    if taleez_object is None:
        return dict(lat=None, lng=None, text="")

    lat = taleez_object.get("lat")
    lat = float(lat) if lat is not None else lat

    lng = taleez_object.get("lng")
    lng = float(lng) if lng is not None else lng

    concatenate = []
    for field in ["postalCode", "city", "country"]:
        if taleez_object.get(field):
            concatenate.append(taleez_object.get(field))

    return dict(
        lat=lat,
        lng=lng,
        text=" ".join(concatenate),
        fields=dict(
            postcode=taleez_object.get("postalCode"),
            city=taleez_object.get("city"),
            country=taleez_object.get("country"),
        ),
    )


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
    taleez_properties = taleez_job.get("properties", [])
    if taleez_tags is None:
        return []
    t = lambda name, value: dict(name=name, value=value)

    custom_fields = [
        "contract",
        "profile",
        "urlApplying",
        "currentStatus",
        "contractLength",
        "contractLengthTimeUnit",
        "fullTime",
        "workHours",
        "remote",
        "recruiterId",
        "unitId",
        "companyLabel",
        "website",
        "visibility",
        "tags",
    ]

    custom_properties = [
        "Département",
        "Niveau de qualification",
        "Salaire",
        "Expérience",
        "Type de contrat",
        "Date de début",
        "Type de télétravail",
    ]

    tags = [
        t("{}_{}".format("taleez", field), taleez_job.get(field))
        for field in custom_fields
    ]
    for prop in custom_properties:
        property_object = next(
            filter(lambda x: x["internal"] == prop, taleez_properties), {}
        )
        prop_value = property_object.get("value") or property_object.get("values")
        tags.append(t("{}_{}".format("taleez", prop), prop_value))

    return tags


def format_job(taleez_job: t.Dict) -> t.Dict:
    job = dict(
        name=taleez_job.get("label", "Undefined"),
        reference=str(taleez_job.get("id")),
        created_at=datetime.fromtimestamp(taleez_job["dateCreation"]).isoformat(),
        updated_at=datetime.fromtimestamp(taleez_job["dateLastPublish"]).isoformat(),
        location=get_location(taleez_job),
        url=taleez_job.get("url"),
        summary=taleez_job.get("jobDescription"),
        sections=get_sections(taleez_job),
        tags=get_tags(taleez_job),
    )
    return job


def get_profile_tags(properties: t.List[t.Dict]) -> t.List[t.Dict]:
    t = lambda name, value: dict(name=name, value=value)

    tags = []

    custom_properties = [
        "Expérience",
        "Niveau d'étude",
        "Salaire €",
        "Disponibilité",
        "Source candidat",
    ]

    for prop in custom_properties:
        property_object = next(filter(lambda x: x["internal"] == prop, properties), {})
        prop_value = property_object.get("value") or property_object.get("values")
        tags.append(t("{}_{}".format("taleez", prop), prop_value))

    return tags


def format_candidate(taleez_candidate: t.Dict) -> t.Dict:
    # TODO: Add skills, educations, experiences based on custom properties mapping
    profile = dict(
        reference=str(taleez_candidate["id"]),
        created_at=datetime.fromtimestamp(taleez_candidate["dateCreation"]).isoformat(),
        info=dict(
            first_name=taleez_candidate.get("firstName"),
            last_name=taleez_candidate.get("lastName"),
            full_name="{} {}".format(
                taleez_candidate.get("firstName"), taleez_candidate.get("lastName")
            ),
            email=taleez_candidate.get("mail"),
            phone=taleez_candidate.get("phone"),
            urls=[
                dict(type=key, url=value)
                for key, value in taleez_candidate.get("social_links", {}).items()
            ],
            location=get_location(taleez_candidate.get("location", None)),
        ),
        skills=[],
        educations=[],
        experiences=[],
        tags=get_profile_tags(taleez_candidate.get("properties", [])),
        resume=dict(raw=taleez_candidate.get("resume")),
    )
    return profile


def format_taleez_object_for_archive(taleez_object: t.Dict) -> t.Dict:
    return dict(reference=str(taleez_object["id"]))


DESCRIPTION = (
    "Taleez est une solution globale de gestion des candidatures et de diffusion"
    " d'offres d'emploi.Pilotez intégralement vos processus de recrutement et intégrez"
    " vos équipes dans les décisions."
)

Taleez = Connector(
    name="Taleez",
    type=ConnectorType.ATS,
    subtype="taleez",
    description=DESCRIPTION,
    url="https://taleez.com/",
    warehouse=TaleezWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=format_taleez_object_for_archive,
        ),
        Flow(Mode.create, Entity.profile, Direction.inbound, format=format_candidate),
        Flow(Mode.update, Entity.profile, Direction.inbound, format=format_candidate),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_taleez_object_for_archive,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_for_update,
        ),
    ),
)
