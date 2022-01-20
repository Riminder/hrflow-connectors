import datetime
import re
from typing import Union


class ParseError(ValueError):
    """
    Parse Error
    """

    def __init__(self, message):
        super().__init__(message)


class TimeDeltaFormatError(ParseError):
    """
    TimeDelta Format Error
    """

    def __init__(self, value):
        super().__init__(
            "Le format de la durée ne respecte pas le format `[[-]DD days, ]HH:MM:SS[.mmmmmm]`. Example : `-1804 days, 15:54:03.999990` or `8:02:12` : `{}`".format(
                value
            )
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


def from_str_to_timedelta(timedelta_str: str) -> datetime.timedelta:
    """
    Convert string to `datetime.timedelta`.
    The date must respect the format `[[-]DD days, ]HH:MM:SS[.mmmmmm]`.
    Examples : `-1804 days, 15:54:03.999990` or `8:02:12`
    Args:
        datetime_str (str): formatted timedelta
    Raises:
        TimeDeltaFormatError: Le format de la durée ne respecte pas le format `[[-]DD days, ]HH:MM:SS[.mmmmmm]`. Example : `-1804 days, 15:54:03.999990` or `8:02:12` : ..."
    Returns:
        datetime.timedelta: converted TimeDelta object
    """
    isoformat_regex = "^((?P<day>[-0-9]+) day(s)?, )?(?P<hour>\d+):(?P<minute>\d+):(?P<second>\d+)(\.(?P<millisecond>\d{1,6}))?$"
    match = re.search(isoformat_regex, timedelta_str)
    if not match:
        raise TimeDeltaFormatError(timedelta_str)

    # Extract value from string
    day, hour, minute, second, millisecond = match.group(
        "day", "hour", "minute", "second", "millisecond"
    )

    # Pad right with 0 the fields millisecond
    # "123" => "123000"
    if millisecond is not None:
        millisecond = millisecond.ljust(6, "0")

    # Convert each field to int
    try:
        day, hour, minute, second, millisecond = (
            any_to_int(day),
            any_to_int(hour),
            any_to_int(minute),
            any_to_int(second),
            any_to_int(millisecond),
        )
    except ValueError:
        raise TimeDeltaFormatError(timedelta_str)

    # Convert to Object
    try:
        return datetime.timedelta(
            days=day,
            hours=hour,
            minutes=minute,
            seconds=second,
            microseconds=millisecond,
        )
    except ValueError:
        raise TimeDeltaFormatError(timedelta_str)
