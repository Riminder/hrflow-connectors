import typing as t
from datetime import datetime
from os.path import join

import requests

from hrflow_connectors.connectors.cornerstoneondemand.schemas import (
    CornerstoneOnDemandAdditionalAttachment,
    CornerstoneOnDemandCandidate,
    CornerstoneOnDemandContactDetails,
    CornerstoneOnDemandCoverLetter,
    CornerstoneOnDemandResume,
)
from hrflow_connectors.connectors.cornerstoneondemand.utils.enums import (
    CornerstoneOnDemandEndpoint,
)
from hrflow_connectors.connectors.hrflow.schemas import (
    Location,
    ProfileInfo,
    RangesDate,
    RangesFloat,
    Section,
)

_CORNERSTONE_ONDEMAND_BASE_API_URL_FORMAT = (
    "https://{corpname}{env}.csod.com/services/api/"
)


def _api_formattable_url_get(endpoint: CornerstoneOnDemandEndpoint) -> str:
    return join(
        _CORNERSTONE_ONDEMAND_BASE_API_URL_FORMAT,
        endpoint,
    )


def _format_datetime(dt: datetime, include_time=False) -> str:
    """
    Converts given datetime object into its string version, following this specific
    format yyyy-mm-ddTHH:MM:SS.

    Args:
      dt (datetime): the datetime object
      include_time (bool): result string also includes the time if true, it does not
        otherwise

    Returns:
      String representation of `dt`.

    Example:
    >>> from datetime import datetime
    >>> _format_datetime(datetime(2023, 11, 2, 14, 15, 16))
    '2023-11-02'
    >>> _format_datetime(datetime(2023, 11, 2, 14, 15, 16), True)
    '2023-11-02T14:15:16'
    >>> _format_datetime(datetime(2023, 11, 2), True)
    '2023-11-02T00:00:00'
    """

    format = "%y-%m-%d"
    if include_time:
        format += "T%H:%M:%S"
    return dt.strftime(format)


def _cornerstone_ondemand_job_ranges_date_get(
    cornerstone_job: t.Dict,
) -> t.Optional[t.List[RangesDate]]:
    """
    Returns:
      List of `Hrflow.RangesDate` objects from `CornerstoneOnDemand.Job`.
    """

    ranges_date = []

    for career_site in cornerstone_job["CareerSites"]:
        name = career_site["Name"]
        value_min = career_site["EffectiveDate"]
        value_max = career_site["ExpirationDate"]
        if name and value_min and value_max:
            ranges_date.append(
                RangesDate(name=name, value_min=value_min, value_max=value_max)
            )

    value_min = cornerstone_job["DefaultEffectiveDate"]
    value_max = cornerstone_job["DefaultExpirationDate"]
    if value_min and value_max:
        ranges_date.append(
            RangesDate(name="Default", value_min=value_min, value_max=value_max)
        )

    if len(ranges_date):
        return ranges_date
    else:
        return None


def _cornerstone_ondemand_job_ranges_float_get(
    cornerstone_job: t.Dict,
) -> t.Optional[t.List[RangesFloat]]:
    """
    Returns:
      List of `HrFlow.RangesFloat` objects from `CornerstoneOnDemand.Job`.
    """

    value_min = cornerstone_job["RangeLow"]
    value_max = cornerstone_job["RangeHigh"]
    unit = cornerstone_job["Currency"]

    if value_min and value_max and unit:
        return [
            RangesFloat(
                name="Compensation", value_min=value_min, value_max=value_max, unit=unit
            )
        ]
    else:
        return None


def _cornerstone_ondemand_general_entities_get(
    object: t.Dict, names: t.List[str], prefix: bool = False
) -> t.Optional[t.List[t.Dict]]:
    """
    Creates a list of `HrFlow.GeneralEntities` from given dictionnary and key names.

    Args:
      object (dict): the source dictionnary
      names (list[str]): the list of keys to look up into `object`
      prefix (bool): for HrFlow tags, it is recommended to use a prefix of the
        connector_name and the name of the original field name joined by an underscore

    Returns:
      List of `HrFlow.GeneralEntities`.

    Example:
      >>> _cornerstone_ondemand_general_entities_get(dict(a=42), ["a"])
      [{'name': 'a', 'value': '42'}]
      >>> _cornerstone_ondemand_general_entities_get(dict(a=42), ["a", "b"])
      [{'name': 'a', 'value': '42'}]
      >>> _cornerstone_ondemand_general_entities_get(dict(a=42), ["a"], True)
      [{'name': 'cornerstone_a', 'value': '42'}]
    """

    entities = [
        dict(name=f"{'cornerstone_' if prefix else ''}{name}", value=str(object[name]))
        for name in names
        if object.get(name)
    ]

    if len(entities):
        return entities
    else:
        return None


def _cornerstone_ondemand_job_languages_get(
    cornerstone_job: t.Dict,
) -> t.Optional[t.List[t.Dict]]:
    """
    Returns:
      List of `HrFlow.GeneralEntities` objects corresponding the
      `CornerstoneOnDemand.Job` languages.
    """

    deflang = cornerstone_job.get("DefaultLanguage")

    if deflang is not None:
        return [dict(name=deflang)]
    else:
        return None


def _cornerstone_ondemand_job_location_get(cornerstone_job: t.Dict) -> Location:
    """
    Returns:
      `HrFlow.Location` object from `CornerstoneOnDemand.Job`
    """

    return Location(
        text=cornerstone_job["Address"], fields=cornerstone_job["AddressDetails"]
    )


def _cornerstone_ondemand_job_sections_get(
    cornerstone_job: t.Dict,
) -> t.Optional[t.List[Section]]:
    """
    Returns:
      List of `HrFlow.Section` objects from `CornerstoneOnDemand.Job`.
    """

    name = cornerstone_job.get("Title")
    title = cornerstone_job.get("MetaPageTitle")
    desc = cornerstone_job.get("MetaPageDesc")

    if name or title or desc:
        return [Section(name=name, title=title, description=desc)]
    else:
        return None


def _cornerstone_ondemand_profile_location_get(cornerstone_profile: t.Dict) -> t.Dict:
    """
    Returns:
      `HrFlow.Location` from `CornerstoneOnDemand.Candidate`.
    """

    text = cornerstone_profile["AddressLine1"]

    if cornerstone_profile["AddressLine2"]:
        text += f', {cornerstone_profile["AddressLine2"]}'

    fields = dict(
        City=cornerstone_profile["City"],
        PostalCode=cornerstone_profile["PostalCode"],
        State=cornerstone_profile["State"],
        Country=cornerstone_profile["Country"],
    )

    return Location(text=text, fields=fields)


def _cornerstone_ondemand_profile_info_get(cornerstone_profile: t.Dict) -> ProfileInfo:
    """
    Returns:
      `HrFlow.ProfileInfo` from `CornerstoneOnDemand.Candidate`.
    """

    return ProfileInfo(
        full_name=cornerstone_profile["Name"],
        first_name=cornerstone_profile["FirstName"],
        last_name=cornerstone_profile["LastName"],
        email=cornerstone_profile["Email"],
        phone=cornerstone_profile["Phone"],
        picture=cornerstone_profile["ThumbImgUrl"],
        gender=cornerstone_profile["Gender"],
        location=_cornerstone_ondemand_profile_location_get(
            cornerstone_profile,
        ),
    )


def _hrflow_profile_candidate_get(
    hrflow_profile: t.Dict,
) -> CornerstoneOnDemandCandidate:
    """
    Returns:
      `CornerstoneOnDemand.Candidate` from `HrFlow.Profile`.
    """

    info = hrflow_profile["info"]
    location = info["location"]
    contact_details = CornerstoneOnDemandContactDetails(
        PhoneNumber=info["phone"], address1=location["text"]
    )

    fields = location.get("fields")
    if isinstance(fields, dict):
        contact_details.city = fields.get("city")
        contact_details.country = fields.get("country")
        contact_details.postalCode = fields.get("postcode")
        contact_details.state = fields.get("state")
        contact_details.address2 = fields.get("text")

    return CornerstoneOnDemandCandidate(
        firstName=info["first_name"],
        lastName=info["last_name"],
        email=info["email"],
        contactDetails=contact_details,
    )


def _hrflow_profile_attachments_get(hrflow_profile: t.Dict) -> t.Optional[t.Dict]:
    """
    Creates a dictionnary with the resume, the cover letter and the all other
    additional attachments from the given `HrFlow.Profile`.

    Returns:
      Dictionnary with all the valid profile attachments.
    """

    result = dict(cover=None, resume=None, additionals=[])

    if not hrflow_profile["attachments"]:
        return None

    for attachment in hrflow_profile["attachments"]:
        file_type = str(attachment.get("type")).lower()
        file_url = attachment.get("public_url")
        if not file_url:
            continue
        try:
            response = requests.get(file_url)
            if response.status_code != requests.codes.ok:
                continue
            file_name = file_url.split("/")[-1]
            file_data = response.content
            if result.get(file_type) is None and file_type in ["resume", "cover"]:
                model = (
                    CornerstoneOnDemandResume
                    if file_type[0] == "r"
                    else CornerstoneOnDemandCoverLetter
                )
                result[file_type] = model(fileName=file_name, file=file_data)
            else:  # additional
                result["additionals"].append(
                    CornerstoneOnDemandAdditionalAttachment(
                        fileName=file_name, file=file_data, id=attachment["alt"]
                    )
                )
        except Exception:
            pass

    return result
