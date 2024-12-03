import typing as t

from hrflow_connectors.v2.connectors.admen.warehouse import AdmenWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def get_job_location(admen_job: t.Dict) -> t.Dict:
    adress_components = []
    for key in ["CITY", "ZIP", "REGION", "PAYS"]:
        if admen_job[key]:
            adress_components.append(admen_job[key])
    return dict(
        text=", ".join(adress_components),
        lat=None,
        lng=None,
        fields=dict(
            city=admen_job["CITY"],
            country=admen_job["PAYS"],
            postcode=admen_job["ZIP"],
            state=admen_job["REGION"],
        ),
    )


def get_job_sections(
    admen_job: t.Dict,
) -> t.List[t.Dict]:
    sections = []
    for section_name in [
        "DESC_MISSION",
        "DESC_CUSTOMER",
        "HTML",
        "POSTIT",
        "COMMENTAIRE",
    ]:
        section = admen_job[section_name]
        if section:
            sections.append(
                dict(name=section_name, title=section_name, description=section)
            )
    return sections


def get_job_tags(admen_job: t.Dict) -> t.List[str]:
    tags = []

    if admen_job["DATE_DEB_RECHERCHE"]:
        tags.append(
            {
                "name": "DATE_DEB_RECHERCHE",
                "value": admen_job["DATE_DEB_RECHERCHE"].strftime("%Y-%m-%d"),
            }
        )
    if admen_job["DATE_PROPOSITION"]:
        tags.append(
            {
                "name": "DATE_PROPOSITION",
                "value": admen_job["DATE_PROPOSITION"].strftime("%Y-%m-%d"),
            }
        )
    if admen_job["DATE_ENTREE_FONCT"]:
        tags.append(
            {
                "name": "DATE_ENTREE_FONCT",
                "value": admen_job["DATE_ENTREE_FONCT"].strftime("%Y-%m-%d"),
            }
        )
    if admen_job["SECTEUR"]:
        tags.append({"name": "SECTEUR", "value": admen_job["SECTEUR"]})
    if admen_job["FONCTION"]:
        tags.append({"name": "FONCTION", "value": admen_job["FONCTION"]})
    if admen_job["TYPEJOB"]:
        tags.append({"name": "TYPEJOB", "value": admen_job["TYPEJOB"]})
    if admen_job["TYPECONTRAT"]:
        tags.append({"name": "TYPECONTRAT", "value": admen_job["TYPECONTRAT"]})
    if admen_job["EDUCLEVEL"]:
        tags.append({"name": "EDUCLEVEL", "value": admen_job["EDUCLEVEL"]})
    if admen_job["JOBEXP"]:
        tags.append({"name": "JOBEXP", "value": admen_job["JOBEXP"]})
    if admen_job["PERIOD"]:
        tags.append({"name": "PERIOD", "value": admen_job["PERIOD"]})
    if admen_job["SALARYMIN"]:
        tags.append({"name": "SALARYMIN", "value": str(admen_job["SALARYMIN"])})
    if admen_job["SALARYMAX"]:
        tags.append({"name": "SALARYMAX", "value": str(admen_job["SALARYMAX"])})
    if admen_job["SALAIRE_ENTREE"]:
        tags.append(
            {"name": "SALAIRE_ENTREE", "value": str(admen_job["SALAIRE_ENTREE"])}
        )
    if admen_job["PROCESS"]:
        tags.append({"name": "PROCESS", "value": admen_job["PROCESS"]})
    return tags


def format_job(
    admen_job: t.Dict,
) -> t.Dict:
    job = dict(
        name=admen_job["LIBELLE"],
        reference=str(admen_job["ID_MISSION"]),
        created_at=admen_job["DATE_SAISIE"].strftime("%Y-%m-%d"),
        updated_at=admen_job["DATE_MAJ"].strftime("%Y-%m-%d"),
        location=get_job_location(admen_job),
        url=admen_job["URL"],
        summary=admen_job["DESC_MISSION"],
        sections=get_job_sections(admen_job),
        tags=get_job_tags(admen_job),
    )
    return job


def get_url(type: str, urls: t.List[t.Dict]) -> t.Optional[str]:
    return next(
        (url["url"] for url in urls if url["type"] == type),
        None,
    )


def format_hrflow_experiences_to_admen(hrflow_experiences: t.List) -> t.List[t.Dict]:
    experiences = []
    for hrflow_experience in hrflow_experiences:
        experience = dict(
            INTITULE_POSTE=hrflow_experience.get("title"),
            RAISON_SOCIALE=hrflow_experience.get("company"),
            POSTE_OCCUPE=hrflow_experience.get("title"),
            DEPARTMENT=hrflow_experience["location"].get("text"),
            DATE_EXP=hrflow_experience["date_start"][:10],
            DATE_FIN=(
                hrflow_experience.get("date_end")[:10]
                if hrflow_experience.get("date_end")
                else None
            ),
            COMMENTAIRES=hrflow_experience.get("description"),
            EN_COURS=1 if not hrflow_experience.get("date_end") else 0,
        )
        experiences.append(experience)
    return experiences


def format_hrflow_profile_to_admen(
    hrflow_profile: t.Dict,
) -> t.Dict:
    hrflow_profile_info = hrflow_profile["info"]

    admen_profile = dict(
        NOM=hrflow_profile_info["last_name"],
        PRENOM=hrflow_profile_info["first_name"],
        EMAIL_PERSO=hrflow_profile_info["email"],
        TEL_MOBILE=hrflow_profile_info["phone"],
        DATE_NAISSANCE=hrflow_profile_info["date_birth"],
        DATE_CREATION=hrflow_profile["created_at"][:10],
        DATE_MAJ=hrflow_profile["updated_at"],
        ADRESSE=hrflow_profile_info["location"]["text"],
        VILLE=hrflow_profile_info["location"]["fields"]["city"],
        CODE_POSTAL=hrflow_profile_info["location"]["fields"]["postcode"],
        AREA=hrflow_profile_info["location"]["fields"]["state"],
        PAYS=hrflow_profile_info["location"]["fields"]["country"],
        LINKEDINPUBLICPROFIL=get_url("linkedin", hrflow_profile_info["urls"]),
        VIADEOPUBLICPROFIL=get_url("viadeo", hrflow_profile_info["urls"]),
        TWITTERPAGE=get_url("twitter", hrflow_profile_info["urls"]),
        FACEBOOKPAGE=get_url("facebook", hrflow_profile_info["urls"]),
        experiences=format_hrflow_experiences_to_admen(hrflow_profile["experiences"]),
        scsync_modified=0,
    )
    return admen_profile


def get_profile_location(admen_profile: t.Dict) -> t.Dict:
    adress_components = []
    for key in ["ADRESSE", "CODE_POSTAL", "VILLE", "AREA", "PAYS"]:
        if admen_profile[key]:
            adress_components.append(admen_profile[key])
    return dict(
        text=admen_profile["ADRESSE"],
        lat=None,
        lng=None,
        fields=dict(
            city=admen_profile["VILLE"],
            country=admen_profile["PAYS"],
            postcode=admen_profile["CODE_POSTAL"],
            state=admen_profile["AREA"],
            text=",".join(adress_components),
        ),
    )


def get_profile_urls(admen_profile: t.Dict) -> t.List[t.Dict]:
    urls = []
    if admen_profile["LINKEDINPUBLICPROFIL"]:
        urls.append(dict(url=admen_profile["LINKEDINPUBLICPROFIL"], type="linkedin"))
    if admen_profile["FACEBOOKPAGE"]:
        urls.append(dict(url=admen_profile["FACEBOOKPAGE"], type="facebook"))
    if admen_profile["TWITTERPAGE"]:
        urls.append(dict(url=admen_profile["TWITTERPAGE"], type="twitter"))
    return urls


def format_profile_gender(admen_gender: t.Optional[str]) -> t.Optional[str]:
    if admen_gender == "F":
        return "female"
    if admen_gender == "M":
        return "male"
    return None


def get_profile_experiences(admen_profile_experiences: t.List) -> t.List[t.Dict]:
    experiences = []

    for admen_experience in admen_profile_experiences:
        if admen_experience["DATE_EXP"]:
            date_start = admen_experience["DATE_EXP"].strftime("%Y-%m-%d")
        else:
            date_start = None
        if admen_experience["DATE_FIN"]:
            date_end = admen_experience["DATE_FIN"].strftime("%Y-%m-%d")
        else:
            date_end = None
        experience = dict(
            title=admen_experience.get("INTITULE_POSTE"),
            company=admen_experience.get("RAISON_SOCIALE"),
            location=dict(
                text=admen_experience.get("VILLE"),
                lat=None,
                lng=None,
            ),
            date_start=date_start,
            date_end=date_end,
            description=admen_experience.get("POSTE_OCCUPE"),
        )
        experiences.append(experience)
    return experiences


def get_profile_tags(admen_profile: t.Dict) -> t.List[str]:
    tags = []
    if admen_profile["NATIONALITE"]:
        tags.append({"name": "NATIONALITE", "value": admen_profile["NATIONALITE"]})
    if admen_profile["SITU_FAMILLE"]:
        tags.append(
            {"name": "SITUATION_FAMILLE", "value": admen_profile["SITU_FAMILLE"]}
        )
    if admen_profile["POSTE_OCCUPE"]:
        tags.append({"name": "POSTE_OCCUPE", "value": admen_profile["POSTE_OCCUPE"]})
    if admen_profile["POSTE_RECHERCHE"]:
        tags.append(
            {"name": "POSTE_RECHERCHE", "value": admen_profile["POSTE_RECHERCHE"]}
        )
    if admen_profile["MOBILITE_TXT"]:
        tags.append({"name": "MOBILITE", "value": admen_profile["MOBILITE_TXT"]})
    if admen_profile["SECTEUR"]:
        tags.append({"name": "SECTEUR", "value": admen_profile["SECTEUR"]})
    return tags


def format_admen_profile_to_hrflow(admen_profile: t.Dict) -> t.Dict:
    admen_profile_experiences = admen_profile["experiences"]
    phone_number = (
        admen_profile["TEL_MOBILE"]
        or admen_profile["MOBILE_PRO"]
        or admen_profile["TEL_STANDARD"]
        or admen_profile["TEL_PERSO"]
    )
    email = admen_profile["EMAIL_PERSO"] or admen_profile["EMAIL"]
    if admen_profile["DATE_NAISSANCE"]:
        date_birth = admen_profile["DATE_NAISSANCE"].strftime("%Y-%m-%d")
    else:
        date_birth = None
    hrflow_profile_info = dict(
        first_name=admen_profile["PRENOM"],
        last_name=admen_profile["NOM"],
        full_name=" ".join([admen_profile["PRENOM"], admen_profile["NOM"]]),
        email=email,
        phone=phone_number,
        date_birth=date_birth,
        location=get_profile_location(admen_profile),
        urls=get_profile_urls(admen_profile),
        gender=format_profile_gender(admen_profile["GENDER"]),
    )
    hrflow_profile = dict(
        reference=str(admen_profile["ID_PERSONNE"]),
        info=hrflow_profile_info,
        resume=admen_profile["resume"],
        experiences=get_profile_experiences(admen_profile_experiences),
        educations=[],
        attachments=[],
        tags=get_profile_tags(admen_profile),
    )
    return hrflow_profile


def format_job_archive_in_hrflow(admen_job: t.Dict) -> t.Dict:
    return dict(reference=str(admen_job["ID_MISSION"]))


def format_profile_archive_in_hrflow(admen_profile: t.Dict) -> t.Dict:
    return dict(reference=str(admen_profile["ID_PERSONNE"]))


def format_hrflow_profile_for_update_to_admen(hrflow_profile: t.Dict) -> t.Dict:
    admen_profile = format_hrflow_profile_to_admen(hrflow_profile)
    admen_profile["ID_PERSONNE"] = int(hrflow_profile["reference"])
    return admen_profile


def format_profile_archive_in_admen(hrflow_profile: t.Dict) -> t.Dict:
    admen_profile = dict(ID_PERSONNE=int(hrflow_profile["reference"]), SOFT_DELETED=1)
    return admen_profile


DESCRIPTION = """
Need an efficient, high-performance HR solution for managing your applications?
Choose AD-Men, the No. 1 software for recruitment agencies.
"""

Admen = Connector(
    name="AD-MEN",
    type=ConnectorType.CRM,
    subtype="admen",
    description=DESCRIPTION,
    url="https://www.ad-rh.com/",
    warehouse=AdmenWarehouse,
    flows=(
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=format_job_archive_in_hrflow,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_admen_profile_to_hrflow,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_admen_profile_to_hrflow,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_profile_archive_in_hrflow,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_to_admen,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=format_hrflow_profile_for_update_to_admen,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.outbound,
            format=format_profile_archive_in_admen,
        ),
    ),
)
