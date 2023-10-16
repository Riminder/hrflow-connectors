import typing as t
from datetime import datetime

from hrflow_connectors.connectors.hrflow.schemas import (
    Location,
    RangesDate,
    RangesFloat,
    Section,
    Skill,
)
from hrflow_connectors.connectors.ukgpro.schemas import (
    UKGProAddress,
    UKGProContactInfo,
    UKGProCountry,
    UKGProEducation,
    UKGProEducationFrom,
    UKGProEducationTo,
    UKGProHyperlink,
    UKGProName,
    UKGProPhone,
    UKGProSchool,
    UKGProWorkExperience,
    UKGProWorkExperienceFrom,
    UKGProWorkExperienceTo,
)


def _select_translation(
    object: t.Optional[t.Dict], locale: str = "en_us"
) -> t.Optional[str]:
    """
    Gets the value of a dict, which key corresponds to the given locale language code

    Args:
      object (Optional[dict]): the dict object
      locale (str): language locale code (by default 'en_us')

    Returns:
      If given locale is found:
        Return value
      Else:
        If 'en_us' is found:
          Return value corresponding to `object['en_us']`
        Else
          If `object` has keys:
            Return the value of the first key
      Return None

    Example:
    >>> _select_translation(dict(fr_ca='Baguette'), 'fr_ca')
    'Baguette'
    >>> _select_translation(dict(fr_ca='Baguette'))
    'Baguette'
    >>> _select_translation(dict(fr_ca='Baguette', ru_ru='Багет'), 'ru_ru')
    'Багет'
    >>> _select_translation(dict())
    >>>
    """

    if not object:  # None or empty
        return None

    if locale in object:
        return str(object[locale])
    elif "en_us" in object:
        return str(object["en_us"])

    return str(list(object.values())[0])


def _ukgpro_job_locale_get(ukgpro_job: dict = None) -> str:
    """
    Get language locale code.

    Args:
      ukgpro_job (dict): `UKGPro.Opportunity` dict object

    Returns:
      Lowercased and underscored language locale code

    Example:
    >>> _ukgpro_job_locale_get(dict(default_locale=dict(code='fr-CA')))
    'fr_ca'
    >>> _ukgpro_job_locale_get()
    'en_us'
    """

    try:
        return ukgpro_job["default_locale"]["code"].lower().replace("-", "_")
    except Exception:
        return "en_us"


def _ukgpro_job_sections_get(
    ukgpro_job: t.Dict, locale: str = "en_us"
) -> t.List[Section]:
    """
    Returns:
      List of `HrFlow.Section` objects from `UKGPro.Opportunity`
    """

    sections = []
    description = ukgpro_job.get("description")
    if not description:
        return sections

    for dtype in ["brief", "detailed"]:
        if not (dtype in description):
            continue
        for scope in ["external", "internal"]:
            if not (scope in description[dtype]):
                continue
            sections.append(
                Section(
                    name="Description",
                    title=f"{dtype} - {scope}",
                    description=_select_translation(description[dtype][scope], locale),
                )
            )

    return sections


def _ukgpro_job_ranges_float_get(ukgpro_job: t.Dict) -> t.List[RangesFloat]:
    """
    Returns:
      List of `HrFlow.RangesFloat` objects from `UKGPro.Opportunity`
    """

    ranges = []

    try:
        compensation = ukgpro_job["compensation"]
        currency = compensation["currency"]
        currency_code = currency["code"]
        ranges.append(
            RangesFloat(
                name="compensation",
                value_min=compensation["pay_rate"],
                value_max=compensation["pay_rate"],
                unit=currency_code,
            )
        )
        compensation_guide = ukgpro_job["compensation_guide"]
        compensation_guide_name = str(compensation_guide["salary_grade_description"])
        ranges.extend(
            [
                RangesFloat(
                    name=compensation_guide_name + " - annual",
                    value_min=compensation_guide["compensation_annual_minimum"],
                    value_max=compensation_guide["compensation_annual_maximum"],
                    unit=currency_code,
                ),
                RangesFloat(
                    name=compensation_guide_name + " - hourly",
                    value_min=compensation_guide["compensation_hourly_minimum"],
                    value_max=compensation_guide["compensation_hourly_maximum"],
                    unit=currency_code,
                ),
            ]
        )
    except KeyError:
        pass

    return ranges


def _ukgpro_job_ranges_date_get(ukgpro_job: t.Dict) -> t.Optional[t.List[RangesDate]]:
    """
    Returns:
      List of `HrFlow.RangesDate` objects from `UKGPro.Opportunity`
    """

    pd = ukgpro_job.get("published_date")
    cd = ukgpro_job.get("closed_date")

    if pd is not None and cd is not None:
        return [RangesDate(name="dates", value_min=pd, value_max=cd)]
    else:
        return None


def _ukgpro_job_location_get(ukgpro_job: t.Dict) -> t.Optional[Location]:
    """
    Returns:
      `HrFlow.Location` from `UKGPro.Opportunity`
    """

    locations = ukgpro_job.get("locations")
    if not isinstance(locations, list):
        return None

    location = locations[0]
    location = Location(
        text=location.get("name"),
        fields=dict(
            city=location.get("city"),
            state=location.get("state"),
            country=(
                location["country"].get("code")
                if isinstance(location.get("country"), dict)
                else None
            ),
        ),
    )

    return location


def _ukgpro_job_skills_get(
    ukgpro_job: t.Dict, locale: str = "en_us"
) -> t.Optional[t.List[Skill]]:
    """
    Returns:
      List of `HrFlow.Skill` objects from `UKGPro.Opportunity`
    """

    skills = []
    skills_ = ukgpro_job.get("skill_criteria")

    if not skills_:
        return None

    for skill in skills_:
        level = skill.get("proficiency_level")
        data = skill.get("skill")
        if level is None or data is None:
            continue
        skills.append(
            Skill(
                name=_select_translation(data.get("name"), locale),
                value=_select_translation(data.get("description"), locale),
                type=_select_translation(level.get("name"), locale),
            )
        )

    return skills


def _ukgpro_job_tags_get(ukgpro_job: t.Dict, locale: str = "en_us") -> t.List[t.Dict]:
    """
    Returns:
      List of `HrFlow.Tag` objects from `UKGPro.Opportunity`.
    """

    tags = [
        dict(name=f"ukgpro_{name}", value=ukgpro_job.get(name))
        for name in [
            "approval_process_type",
            "employee_type",
            "hours_per_shift",
            "hours_per_week",
            "id",
            "is_budgeted",
            "is_continuous_opening",
            "is_featured",
            "opening_reason",
            "priority",
            "request_candidate_availability",
            "staffing_plan_impact",
            "status",
            "target_start_date",
        ]
        if ukgpro_job.get(name)
    ]

    travel = ukgpro_job.get("travel")
    if isinstance(travel, dict):
        tags.append(
            dict(
                name="ukgpro_travel_is_required",
                value=str(travel.get("is_required")),
            )
        )
        details = travel.get("details")
        if isinstance(details, dict):
            tags.append(
                dict(
                    name="ukgpro_travel_details",
                    value=_select_translation(details, locale),
                )
            )

    for key in ["company", "job_family"]:
        value = ukgpro_job.get(key)
        if not isinstance(value, dict):
            continue
        tags.append(
            dict(
                name=f"ukgpro_{key}",
                value=_select_translation(
                    value.get("name"),
                    locale,
                ),
            )
        )

    return tags


def _ukgpro_job_metadatas_work_experiences_get(
    ukgpro_job: t.Dict, locale: str = "en_us"
) -> t.List[t.Dict]:
    """
    Returns:
      List of `HrFlow.Metadatas` regarding working experiences from a
      `UKGPro.Opportunity`
    """

    metadatas = []
    work_experiences = ukgpro_job.get("work_experience_criteria")

    if not isinstance(work_experiences, list):
        return metadatas

    for ii, experience in enumerate(work_experiences):
        metadatas.extend(
            [
                dict(
                    name=f"work_experience_criteria_{ii}_is_required",
                    value=str(experience.get("is_required")),
                ),
                dict(
                    name=f"work_experience_criteria_{ii}_description",
                    value=_select_translation(experience.get("description"), locale),
                ),
            ]
        )

        for key in ["minimum", "maximum"]:
            value = experience.get(f"{key}_required_years")
            if value:
                metadatas.append(
                    dict(
                        name=f"work_experience_{ii}_{key}_required_years",
                        value=str(value),
                    )
                )

    return metadatas


def _ukgpro_job_metadatas_educations_get(
    ukgpro_job: t.Dict, locale: str = "en_us"
) -> t.List[t.Dict]:
    """
    Returns:
      List of `HrFlow.GeneralEntity` regarding educations from a `UKGPro.Opportunity`
    """

    metadatas = []
    educations = ukgpro_job.get("education_criteria")
    if not educations:
        return metadatas

    for ii, education in enumerate(educations):
        metadatas.extend(
            [
                dict(
                    name=f"education_{ii}_is_required",
                    value=str(education.get("is_required")),
                ),
                dict(
                    name=f"education_{ii}_allow_related",
                    value=str(education.get("allow_related")),
                ),
            ]
        )

        for key in ["degree", "major"]:
            nested = education.get(key)
            if not nested:
                continue
            translations = nested.get("name")
            if translations:
                metadatas.append(
                    dict(
                        name=f"education_{ii}_{key}",
                        value=str(
                            _select_translation(
                                translations,
                                locale,
                            )
                        ),
                    )
                )

    return metadatas


def _ukgpro_job_metadatas_behaviors_and_motivations_get(
    ukgpro_job: t.Dict, locale: str = "en_us"
) -> t.List[t.Dict]:
    """
    Returns:
      List of `HrFlow.GeneralEntity` objects regarding behaviours and motivations from
      `UKGPro.Opportunity`
    """

    metadatas = []

    for key in ["behavior", "motivation"]:
        wrapper = ukgpro_job.get(f"{key}_criteria")
        if not isinstance(wrapper, list):
            continue
        for ii, object in enumerate(wrapper):
            metadatas.append(
                dict(
                    name=f"{key}_{ii}_is_required",
                    value=str(object.get("is_required")),
                )
            )
            kernel = object.get(key)
            if not isinstance(kernel, dict):
                continue
            for key2 in ["name", "description"]:
                metadatas.append(
                    dict(
                        name=f"{key}_{ii}_{key2}",
                        value=_select_translation(kernel.get(key2), locale),
                    )
                )

    return metadatas


def _ukgpro_job_metadatas_get(
    ukgpro_job: t.Dict, locale: str = "en_us"
) -> t.List[t.Dict]:
    """
    Returns:
      List of `HrFlow.GeneralEntity` from a `UKGPro.Opportunity`
    """

    return [
        *_ukgpro_job_metadatas_work_experiences_get(ukgpro_job, locale),
        *_ukgpro_job_metadatas_educations_get(ukgpro_job, locale),
        *_ukgpro_job_metadatas_behaviors_and_motivations_get(
            ukgpro_job,
            locale,
        ),
    ]


def _hrflow_profile_name_get(hrflow_profile: t.Dict) -> t.Optional[UKGProName]:
    """
    Returns:
      `UKGPro.Name` from a `HrFlow.ProfileInfo`
    """

    info = hrflow_profile.get("info")

    if isinstance(info, dict):
        return UKGProName(first=info.get("first_name"), last=info.get("last_name"))
    else:
        return None


def _hrflow_profile_info_address_get(info: t.Dict) -> t.Optional[UKGProAddress]:
    """
    Returns:
      `UKGPro.Address` from a `HrFlow.ProfileInfo`
    """

    location = info["location"]

    if not isinstance(location, dict):
        return None

    address = UKGProAddress(line1=location["text"])
    fields = location["fields"]

    if isinstance(fields, dict):
        address.city = fields.get("city")
        address.country = UKGProCountry(code=fields.get("country"))
        address.postal_code = fields.get("postcode")
        address.state = fields.get("state")

    return address


def _hrflow_profile_contact_info_get(
    hrflow_profile: t.Dict,
) -> t.Optional[UKGProContactInfo]:
    """
    Returns:
      `UKGPro.ContactInfo` from a `HrFlow.ProfileInfo`
    """

    info = hrflow_profile.get("info")

    if isinstance(info, dict):
        return UKGProContactInfo(
            email=info.get("email"),
            address=_hrflow_profile_info_address_get(info),
            phone=UKGProPhone(primary=info.get("phone")),
        )
    else:
        return None


def _hrflow_profile_hyperlinks_get(
    hrflow_profile: t.Dict,
) -> t.Optional[t.List[UKGProHyperlink]]:
    """
    Returns:
      List of `UKGPro.Hyperlink` objects from `HrFlow.ProfileInfo`
    """

    info = hrflow_profile.get("info")

    if not isinstance(info, dict):
        return None

    urls_ = info.get("urls")

    if urls_ is None:
        return None

    # there are 2 types of urls object
    # - dict[from_resume:list[str], github:str, linkedin:str, twitter:str, facebook:str]
    # - list[dict(type:str, url:str)]

    urls = []

    if info.get("picture"):
        urls.append(
            UKGProHyperlink(name="picture", url=info.get("picture")),
        )

    if isinstance(urls_, list):
        urls.extend(
            [
                UKGProHyperlink(name=f"hyperlink_{ii}", url=object.get("url"))
                for ii, object in enumerate(urls_)
                if isinstance(object, dict)
            ]
        )
    elif isinstance(urls_, dict):
        if urls_.get("from_resume"):
            urls.extend(
                [
                    UKGProHyperlink(name=f"from_resume_{ii}", url=url)
                    for ii, url in enumerate(urls_["from_resume"])
                ]
            )

        urls.extend(
            [
                UKGProHyperlink(name=name, url=urls_.get(name))
                for name in ["linkedin", "twitter", "facebook", "github"]
                if urls_.get(name)
            ]
        )

    return urls


def _hrflow_profile_skills_get(hrflow_profile: t.Dict) -> None:
    """
    Returns:
      List of `UKGPro.Skill` from a `HrFlow.ProfileInfo`
    """

    # TODO: get proeficiency level ids


def _hrflow_profile_from_and_to_get(
    date_start: t.Optional[str], date_end: t.Optional[str], is_experience: bool = True
) -> t.Dict:
    """
    Gets `date_start` (resp. `date_end`) month and year

    Returns:
      dict(from:`UKGPro.(Experience|Education)From`,
      to:`UKGPro.(Experience|Education)To`) from a `HrFlow.(Experience|Education)`
    """

    result = dict(from_=None, to=None)
    try:
        date_start = datetime(date_start[:16])
        date_end = datetime(date_end[:16])
    except Exception:
        return result

    model_from = UKGProWorkExperienceFrom
    model_to = UKGProWorkExperienceTo

    if not is_experience:
        model_from = UKGProEducationFrom
        model_to = UKGProEducationTo

    result.from_ = model_from(month=date_start.month, year=date_start.year)
    result.to = model_to(month=date_end.month, year=date_end.year)

    return result


def _hrflow_profile_enrich_description(object: t.Dict) -> str:
    """
    Returns:
      str representing a description of skills, certifications, courses, tasks,
      languages and interests from an object.
    """

    description = ""

    if object.get("description"):
        description += object["description"] + "."

    skills = object.get("skills")
    if skills:
        if not isinstance(skills, list):
            skills = [skills]
        if len(skills) > 0:
            description += (
                " Here is a list of skills, names, and optionally their types and"
                " values: "
                + ", ".join(
                    s["name"]
                    + (f" of type '{s.get('type')}'" if s.get("type") else "")
                    + (f" and value '{s.get('value')}'" if s.get("value") else "")
                    for s in skills
                )
                + "."
            )

    for key in ["certifications", "courses", "tasks", "languages", "interests"]:
        data = object.get(key)
        if data is None:
            continue
        if not isinstance(data, list):
            data = [data]
        if len(data) == 0:
            continue
        description += (
            f" Here is a list of {key}, names, and optionally their values: "
            + ", ".join(
                f"'{d['name']}'"
                + (f" and its value '{d.get('value')}'" if d.get("value") else "")
                for d in data
            )
            + "."
        )

    return description


def _hrflow_profile_experiences_get(
    hrflow_profile: t.Dict,
) -> t.Optional[t.List[UKGProWorkExperience]]:
    """
    Returns:
      List of `UKGPro.WorkExperience` objects from `HrFlow.ProfileInfo`
    """

    experiences = hrflow_profile.get("experiences")

    if not isinstance(experiences, list):
        return None
    else:
        return [
            UKGProWorkExperience(
                job_title=experience.get("title"),
                company=experience.get("company"),
                description=_hrflow_profile_enrich_description(experience),
                **_hrflow_profile_from_and_to_get(
                    experience.get("date_start"),
                    experience.get("date_end"),
                    is_experience=True,
                ),
            )
            for experience in experiences
        ]


def _hrflow_profile_educations_get(
    hrflow_profile: t.Dict,
) -> t.Optional[t.List[UKGProEducation]]:
    """
    Returns:
      List of `UKGPro.Education` objects from `HrFlow.ProfileInfo`
    """

    educations = hrflow_profile.get("educations")

    if not isinstance(educations, list):
        return None
    else:
        return [
            UKGProEducation(
                school=UKGProSchool(name=education.get("school")),
                description=_hrflow_profile_enrich_description(education),
                **_hrflow_profile_from_and_to_get(
                    education.get("date_start"),
                    education.get("date_end"),
                    is_experience=False,
                ),
            )
            for education in educations
        ]
