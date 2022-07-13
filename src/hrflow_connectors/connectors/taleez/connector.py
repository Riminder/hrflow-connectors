from datetime import datetime
import json
import typing as t


from hrflow_connectors.connectors.hrflow.warehouse import HrFlowProfileWarehouse

from hrflow_connectors.connectors.taleez.warehouse import TaleezProfilesWarehouse

from hrflow_connectors.core import BaseActionParameters, Connector, ConnectorAction

PROPERTIES_PATH = "src/hrflow_connectors/connectors/taleez/properties.json"
DATE_FORMAT = "%Y-%m-%d"
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


class ExperienceId:
    def __init__(self, ids, period):
        self.ids = ids
        self.period = period

    def get_period(self):
        return self.period

    def get_ids(self):
        return self.ids


EXPERIENCE_IDS = ExperienceId(ids=[600932 + i for i in range(10)], period=1)


def process_experience(tag_value):
    """This function parses the experience duration of the candidate
    from the HrFlow.ai profile and returns the corresponding id for the experience
    property

    Args:
        profile (t.Dict): HrFlow.ai profile to process

    Returns:
        int: experience id for the candidate
    """
    t = float(tag_value)
    T = EXPERIENCE_IDS.get_period()
    ids = EXPERIENCE_IDS.get_ids()
    n = 0
    if t > 0 and t < T:
        n = 1
    elif t < (len(ids) - 1) * T:
        n = int(t)
    else:
        n = T
    return ids[n]


def process_availability(tag_value):
    """This function parses the text of the availability tag in HrFlow.ai
        Profile and returns the corresponding availability choice id in the property

    Args:
        tag_value (str): value of the tag with name availabilty in HrFlow.ai Profile.

    Returns:
        int: choice id from the picklist defined in the property Disponibilité in Taleez
    """
    try:
        datetime.strptime(tag_value, DATE_FORMAT)
        return tag_value
    except ValueError:
        return ""


def process_source(tag_value):
    return tag_value if tag_value else DEFAULT_CANDIDATE_SOURCE


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


# Taleez properties
def get_property_by_id(
    properties_list: t.List[t.Dict], prop_id: int
) -> t.Optional[t.Dict]:
    return next(iter(filter(lambda x: x["id"] == prop_id, properties_list)), None)


# Get settings
def tags_to_properties_mapping(properties: t.List[t.Dict]) -> t.Dict:
    mapping = dict()
    for tag_name, id_ in HRFLOW_TAGS_MAPPING.items():
        p = get_property_by_id(properties, id_)
        mapping[tag_name] = {"id": id_, "key": p["key"], "type": p["type"]}

    return mapping


# Taleez Property Class
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


# Hardskills
def get_parsed_hardskills(profile: t.Dict):
    skills = list(filter(lambda x: x["type"] == "hard", profile["skills"]))
    # return ", ".join(sorted(set(skills), key=skills.index))
    return ", ".join(sorted(skills)) if skills else ""


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


# Get properties to push on taleez from HrFlow.ai profile object
def get_hrflow_tag_by_name(hrflow_profile: t.Dict, tag_name: str) -> t.Optional[t.Dict]:
    return next(filter(lambda x: x["name"] == tag_name, hrflow_profile["tags"]), None)


#
def get_profile_properties_to_push(
    profile: t.Dict, origin_properties: t.List[t.Dict]
) -> t.List[t.Dict]:
    """Takes hrflow profile object and the list of all properties from Taleez candidates endpoint
       Generates Property objects to leverage class methods to format tag values and each tag specific
       processor to format the text if needed or select the corresponding choice from choices list in case
       of a select field depending on the rule fixed in the processor function.

    Args:
        profile (t.Dict): HrFlow.ai Profile
        origin_properties (t.List[t.Dict]): List of properties created by the customer on Taleez (result of get candidate-properties)

    Returns:
        t.List[t.Dict]: List of properties payload to post via candidate-properties endpoint
    """
    output = []

    # Default Values
    # Source

    property_objects = dict()
    for tag_name, property_id in HRFLOW_TAGS_MAPPING.items():
        property_dict = get_property_by_id(
            origin_properties, property_id
        )  # handle experience and skills/certifications
        if property_dict:
            property_objects[tag_name] = Property(**property_dict)

    # 1 - Availability
    tag_name = "availability"
    input_text = get_hrflow_tag_by_name(profile, tag_name)["value"]
    processor = process_availability
    output.append(
        property_objects[tag_name].generate_property_payload(
            input_text=input_text, processor=processor
        )
    )
    # 2 - Experience
    tag_name = "experience"  # noqa it's not a tag in HrFlow profile
    input_text = profile["experiences_duration"]  # noqa input_text here is a float
    processor = process_experience
    output.append(
        property_objects[tag_name].generate_property_payload(
            input_text=input_text, processor=processor
        )
    )
    # 3 - Source candidate (HrFlow)
    tag_name = "source_candidate"
    input_text = DEFAULT_CANDIDATE_SOURCE  # HrFlow
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )  # TODO handle case tag doesn't exist --> input_text=None
    #  - Hardskills
    tag_name = "hard_skills"  # noqa it's not a tag in HrFlow profile
    # Skills retrieved from job board (in tags as text concatenated by ",")
    input_text = get_parsed_hardskills(profile)
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )
    # 4 - Languages
    tag_name = "languages"
    input_text = get_languages(profile)
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )
    # 5 - Courses
    tag_name = "courses"
    input_text = get_courses(profile)
    output.append(
        property_objects[tag_name].generate_property_payload(input_text=input_text)
    )
    return output


def format_profile(profile: t.Dict) -> t.Dict:
    candidate = dict(
        first_name=profile["info"]["first_name"],
        last_name=profile["info"]["last_name"],
        mail=profile["info"]["email"],
        initial_referrer="",
        lang=str(profile["text_language"]).upper(),
        social_links={},
    )
    with open(PROPERTIES_PATH) as f:
        properties = get_profile_properties_to_push(profile, json.load(f)["list"])
    cv = get_CV(profile)
    return dict(candidate=candidate, CV=cv, properties=properties)


Taleez = Connector(
    name="Taleez",
    description="",
    url="https://taleez.com/",
    actions=[
        ConnectorAction(
            name="push_profiles",
            description=(
                "Retrieves profiles from HrFlow Sources and posts them to Taleez ATS "
                "enriching them with properties extracted from the profile"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=TaleezProfilesWarehouse,
        ),
    ],
)
