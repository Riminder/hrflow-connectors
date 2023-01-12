import datetime
import re
from typing import Union


class ParseError(ValueError):
    """
    Parse Error
    """

    def __init__(self, message):
        super().__init__(message)


class DateFormatError(ParseError):
    """
    Date Format Error
    """

    def __init__(self, value):
        super().__init__(
            "Le format de la date ne respecte pas la norme ISO8601 : `{}`".format(value)
        )


def any_to_int(any: Union[None, str]) -> int:
    """
    Convert int string to integer. if this value is `None`, return `0`
    Args:
        any (Union[None, str]): value to convert
    Returns:
        int: converted integer
    """
    if any is None:
        return 0
    return int(any)


def from_str_to_datetime(datetime_str: str) -> datetime.datetime:
    """
    Convert string to `datetime.datetime`.
    The date must respect ISO8601 format.
    Args:
        datetime_str (str): date formatted with the norme ISO8601
    Raises:
        DateFormatError: "Le format de la date ne respecte pas la norme ISO8601 : ..."
    Returns:
        datetime.datetime: converted Datetime object
    """
    isoformat_regex = r"""^(?P<year>\d{4})(-|/)(?P<month>\d{2})(-|/)(?P<day>\d{2})(.(?P<hour>\d{2})
            (:(?P<minute>\d{2})(:(?P<second>\d{2})(\.(?P<millisecond>\d{1,6}))?)?)?)?(?P<tz>((?P<tz_symbol>-|\+)
            (?P<tz_hour>\d{2}):?(?P<tz_minute>\d{2})(:(?P<tz_second>\d{2})
            (\.(?P<tz_millisecond>\d{1,6}))?)?)|Z)?$"""

    match = re.search(isoformat_regex, datetime_str)
    if not match:
        raise DateFormatError(datetime_str)

    # Extract value from string
    # Datetime
    year, month, day = match.group("year", "month", "day")
    hour, minute, second, millisecond = match.group(
        "hour", "minute", "second", "millisecond"
    )

    # TimeZone
    tz_field_in_str = match.group("tz")  # example : +02:00
    tz_symbol = match.group("tz_symbol")
    tz_hour, tz_minute, tz_second, tz_millisecond = match.group(
        "tz_hour", "tz_minute", "tz_second", "tz_millisecond"
    )

    # Pad right with 0 the fields millisecond and tz_millisecond
    # "123" => "123000"
    if millisecond is not None:
        millisecond = millisecond.ljust(6, "0")
    if tz_millisecond is not None:
        tz_millisecond = tz_millisecond.ljust(6, "0")

    # Convert each field to int
    try:
        year, month, day = any_to_int(year), any_to_int(month), any_to_int(day)
        hour, minute, second, millisecond = (
            any_to_int(hour),
            any_to_int(minute),
            any_to_int(second),
            any_to_int(millisecond),
        )
        tz_hour, tz_minute, tz_second, tz_millisecond = (
            any_to_int(tz_hour),
            any_to_int(tz_minute),
            any_to_int(tz_second),
            any_to_int(tz_millisecond),
        )
    except ValueError:
        raise DateFormatError(datetime_str)

    # Convert to Object
    delta = datetime.timedelta(
        hours=tz_hour, minutes=tz_minute, seconds=tz_second, microseconds=tz_millisecond
    )
    if tz_symbol == "-":
        # when -HH:MM
        delta = -1 * delta

    timezone = datetime.timezone(delta)

    if tz_field_in_str is None:
        timezone = None

    try:
        return datetime.datetime(
            year=year,
            month=month,
            day=day,
            hour=hour,
            minute=minute,
            second=second,
            microsecond=millisecond,
            tzinfo=timezone,
        )
    except ValueError:
        raise DateFormatError(datetime_str)
