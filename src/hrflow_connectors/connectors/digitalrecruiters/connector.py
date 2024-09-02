import re
import typing as t
from datetime import datetime

from hrflow_connectors.connectors.digitalrecruiters.warehouse import (
    DigitalRecruitersJobWarehouse,
    DigitalRecruitersReadProfilesWarehouse,
    DigitalRecruitersWriteProfileWarehouse,
)
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileParsingWarehouse,
    HrFlowProfileWarehouse,
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


def html_to_plain_text(html_text):
    if html_text is None:
        return None
    # Remove HTML tags
    plain_text = re.sub(r"<.*?>", "", html_text)

    # Replace special characters with their plain text equivalents
    plain_text = plain_text.replace("&nbsp;", " ")
    plain_text = plain_text.replace("&amp;", "&")
    plain_text = plain_text.replace("&quot;", '"')
    plain_text = plain_text.replace("&apos;", "'")
    plain_text = plain_text.replace("&lt;", "<")
    plain_text = plain_text.replace("&gt;", ">")

    # Remove extra whitespace and newline characters
    plain_text = re.sub(r"\s+", " ", plain_text).strip()

    return plain_text


def get_job_location(digital_recruiters_adress: t.Union[t.Dict, None]) -> t.Dict:
    if not digital_recruiters_adress:
        return dict(lat=None, lng=None, text="")

    lat = digital_recruiters_adress.get("position", {}).get("lat", None)
    lat = float(lat) if lat is not None else lat

    lng = digital_recruiters_adress.get("position", {}).get("lon", None)
    lng = float(lng) if lng is not None else lng
    text = digital_recruiters_adress.get("formatted", None)

    return dict(lat=lat, lng=lng, text=text)


def get_sections(digital_recruiters_job: t.Dict) -> t.List[t.Dict]:
    sections = []
    if (
        "description" not in digital_recruiters_job
        or "profile" not in digital_recruiters_job
    ):
        return sections

    for section_name in [
        "description",
        "profile",
    ]:
        section = digital_recruiters_job.get(section_name, None)
        if section is not None:
            sections.append(
                dict(
                    name=section_name,
                    title=section_name,
                    description=html_to_plain_text(section),
                )
            )
    return sections


def get_tags(digital_recruiters_job: t.Dict) -> t.List[t.Dict]:
    job = digital_recruiters_job
    custom_field_mapping = {
        "Possibilité de télétravail": "digitalrecruiters_possibilite_de_teletravail",
        "Automatisation (HRFlow.ai)": "digitalrecruiters_automatisation_hrflow",
        "Heures hebdomadaires": "digitalrecruiters_heures_hebdomadaires",
        "Date envisagée de recrutement": (
            "digitalrecruiters_date_enviseagee_de_recrutement"
        ),
        "Date de fin": "digitalrecruiters_date_de_fin",
        "Motif de recrutement": "digitalrecruiters_motif_de_recrutement",
        "Nom de la personne remplacée": (
            "digitalrecruiters_nom_de_la_personne_remplacee"
        ),
        "Echelon": "digitalrecruiters_echelon",
        "Filière": "digitalrecruiters_filiere",
        "Horaires": "digitalrecruiters_horaires",
        "Un candidat est déjà identifié": "digitalrecruiters_candidat_deja_identifie",
        "Nom de ce candidat": "digitalrecruiters_nom_du_candidat",
    }

    tags = []

    def add_tag(name, value):
        if value is not None and value != "":
            tags.append({"name": name, "value": value})

    compensation = job.get("salary", {})
    if compensation:
        add_tag("digitalrecruiters_compensation_min", compensation.get("min", None))
        add_tag("digitalrecruiters_compensation_max", compensation.get("max", None))
        add_tag(
            "digitalrecruiters_compensation_currency",
            compensation.get("currency", None),
        )

    manager = job.get("entity", {}).get("manager", {})
    if manager:
        add_tag("digitalrecruiters_manager_firstName", manager.get("firstname", None))
        add_tag("digitalrecruiters_manager_lastName", manager.get("lastname", None))
        add_tag("digitalrecruiters_manager_position", manager.get("position", None))
        add_tag("digitalrecruiters_manager_picture", manager.get("picture_url", None))

    recruiter = job.get("referent_recruiter", {})
    if recruiter:
        add_tag("digitalrecruiters_recruiter_email", recruiter.get("email", None))
        add_tag(
            "digitalrecruiters_recruiter_phoneNumber",
            recruiter.get("phoneNumber", None),
        )
        add_tag(
            "digitalrecruiters_recruiter_picture", recruiter.get("picture_url", None)
        )

    hierarchy_list = digital_recruiters_job.get("hierarchy", [])
    for item in hierarchy_list:
        depth = item.get("depth", None)
        column_name = item.get("column_name", None)
        public_name = item.get("public_name", None)
        add_tag(f"hierarchy_{depth}", f"{column_name}:{public_name}")

    custom_fields = job.get("custom_fields", [])
    if custom_fields:
        for custom_field in custom_fields:
            name = custom_field.get("name", None)
            value = custom_field.get("value", None)
            mapped_name = custom_field_mapping.get(name, None)
            if mapped_name:
                add_tag(mapped_name, value)

    return tags


def format_skills(skills_list):
    formatted_skills = [
        {"name": skill, "type": None, "value": None} for skill in skills_list
    ]
    return formatted_skills


# format profile location retrieved from DigitlRecruiters
def get_profile_location(Dr_location: t.Dict) -> t.Dict:
    if not Dr_location:
        return dict(text="", lat=None, lng=None)
    street = Dr_location.get("street", "")
    zip_code = Dr_location.get("zip", "")
    city = Dr_location.get("city", "")
    country = Dr_location.get("country", "")

    parts = []

    if street:
        parts.append(street)

    if city:
        parts.append(city)

    if zip_code:
        parts.append(f"({zip_code})")

    if country:
        parts.append(country)

    location_text = ", ".join(parts)
    location_lat = Dr_location.get("latitude", None)
    location_lng = Dr_location.get("longitude", None)
    return dict(text=location_text, lat=location_lat, lng=location_lng)


def normalize_link(link):
    normalized_link = link.replace("\\", "")
    return normalized_link


def format_job(digital_recruiters_job: t.Dict) -> t.Dict:
    picture = None
    pictures = digital_recruiters_job.get("pictures", [])
    if pictures:
        picture = pictures[0].get("default", None)
    job = dict(
        name=digital_recruiters_job.get("title", None),
        picture=picture,
        reference=digital_recruiters_job.get("reference", None),
        created_at=digital_recruiters_job.get("published_at", None),
        location=get_job_location(digital_recruiters_job.get("address", {})),
        sections=get_sections(digital_recruiters_job),
        requirements=html_to_plain_text(digital_recruiters_job.get("profile", None)),
        skills=format_skills(digital_recruiters_job.get("skills", [])),
        tags=get_tags(digital_recruiters_job),
    )
    return job


def format_dr_profile(dr_candidate: t.Dict) -> t.Dict:
    full_name = (
        f"{dr_candidate.get('firstName', None)} {dr_candidate.get('lastName', None)}"
    )
    resume = dr_candidate.get("cv", {}).get("url", None)
    resume = normalize_link(resume)
    avatar = dr_candidate.get("avatar", {}).get("url", None)
    reference = dr_candidate.get("id")
    created_at = dr_candidate.get("createdAt")
    resume = dr_candidate.get("resume")
    location = get_profile_location(dr_candidate.get("location", {}))
    # add tags
    tags = []

    def add_tag(name, value):
        if value is not None:
            tags.append({"name": name, "value": value})

    add_tag("digitalrecruiters_profile-email", dr_candidate.get("email", None))
    add_tag(
        "digitalrecruiters_profile-phoneNumber", dr_candidate.get("phoneNumber", None)
    )
    add_tag("digitalrecruiters_profile-fullName", full_name)
    add_tag("digitalrecruiters_avatar", avatar)
    add_tag("digitalrecruiters_profile-resume", resume)
    add_tag("digitalrecruiters_profile-location", location.get("text", None))
    add_tag(
        "digitalrecruiters_education-level", dr_candidate.get("educationLevel", None)
    )
    add_tag(
        "digitalrecruiters_job-experience-level",
        dr_candidate.get("experienceLevel", None),
    )
    add_tag("digitalrecruiters_job-title", dr_candidate.get("jobTitle", None))
    add_tag("digitalrecruiters_job-id", dr_candidate.get("jobAd", {}).get("id", None))
    add_tag(
        "digitalrecruiters_job-published-at",
        dr_candidate.get("jobAd", {}).get("publishedAt", None),
    )
    add_tag("digitalrecruiters_locale", dr_candidate.get("locale", None))
    add_tag("digitalrecruiters_origin", dr_candidate.get("origin", None))
    add_tag("digitalrecruiters_is-spontaneous", dr_candidate.get("isSpontaneous", None))
    add_tag("digitalrecruiters_is-imported", dr_candidate.get("isImported", None))
    add_tag(
        "digitalrecruiters_is-from-external-api",
        dr_candidate.get("isFromExternalApi", None),
    )
    add_tag(
        "digitalrecruiters_rejected-reason", dr_candidate.get("rejectedReason", None)
    )
    add_tag(
        "digitalrecruiters_application-status",
        dr_candidate.get("applicationStatus", None),
    )

    metadatas = []
    profile_hrflow = dict(
        reference=reference,
        created_at=created_at,
        updated_at=datetime.utcnow().isoformat(),
        resume=resume,
        tags=tags,
        metadatas=metadatas,
    )
    return profile_hrflow


def format_profile(profile_hrflow: t.Dict) -> t.Dict:
    dr_profile_dict = dict()

    # Gender mapping: 1 for Male, 2 for Female
    dr_profile_dict["gender"] = (
        1 if profile_hrflow["info"]["gender"].lower() == "male" else 2
    )
    dr_profile_dict["firstName"] = profile_hrflow["info"]["first_name"]
    dr_profile_dict["lastName"] = profile_hrflow["info"]["last_name"]
    dr_profile_dict["email"] = profile_hrflow["info"]["email"]
    dr_profile_dict["phoneNumber"] = profile_hrflow["info"].get("phone")
    location = profile_hrflow["info"].get("location", {})
    dr_profile_dict["addressStreet"] = location.get("text")

    fields = location.get("fields", {})
    if isinstance(fields, list):
        # If fields is a list, take the first element as it's a dictionary
        fields = fields[0]
    dr_profile_dict["addressZip"] = fields.get("postcode")
    dr_profile_dict["addressCity"] = fields.get("state_district")

    profile = dict()
    profile["consent_date"] = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    profile["s_o"] = profile_hrflow.get("s_o", "")
    profile["locale"] = "fr_FR"
    profile["ApplicationProfile"] = dr_profile_dict

    return profile


DESCRIPTION = (
    "Digital Recruiters: Tech-driven hiring platform with job posting,"
    " automation, and analytics. Simplify recruitment, reduce time-to-hire,"
    " and elevate candidate experience. Streamline the hiring process for"
    " businesses with advanced features and integration capabilities."
)

DigitalRecruiters = Connector(
    name="DigitalRecruiters",
    type=ConnectorType.ATS,
    description=DESCRIPTION,
    url="https://www.digitalrecruiters.com/",
    actions=[
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all jobs from Digital Recruiters and sends them to an"
                " Hrflow.ai Board."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobsActionParameters", format=format_job
            ),
            origin=DigitalRecruitersJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves all profiles from Digital Recruiters and sends them to an"
                " Hrflow.ai Source."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfilesActionParameters", format=format_dr_profile
            ),
            origin=DigitalRecruitersReadProfilesWarehouse,
            target=HrFlowProfileParsingWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description="Pushes a profile from Hrflow.ai to Digital Recruiters.",
            parameters=BaseActionParameters.with_defaults(
                "WriteProfilesActionParameters", format=format_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=DigitalRecruitersWriteProfileWarehouse,
            action_type=ActionType.outbound,
        ),
    ],
)
