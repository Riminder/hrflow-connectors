import datetime
from hrflow_connectors.utils.datetime_converter import (
    from_str_to_datetime,
    DateFormatError,
)


## YYYY/MM/DD
def test_simple_slash_date():
    got = from_str_to_datetime("2000/10/01")
    expected = datetime.datetime(2000, 10, 1, 0, 0)
    assert got == expected


### Separation : Space
def test_simple_slash_date_space_with_hours():
    got = from_str_to_datetime("2000/10/01 12")
    expected = datetime.datetime(2000, 10, 1, 12, 0)
    assert got == expected


def test_simple_slash_date_space_with_hours_minutes():
    got = from_str_to_datetime("2000/10/01 12:02")
    expected = datetime.datetime(2000, 10, 1, 12, 2)
    assert got == expected


def test_simple_slash_date_space_with_hours_minutes_seconds():
    got = from_str_to_datetime("2000/10/01 12:02:14")
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14)
    assert got == expected


### Separation : T
def test_simple_slash_date_T_with_hours():
    got = from_str_to_datetime("2000/10/01T12")
    expected = datetime.datetime(2000, 10, 1, 12, 0)
    assert got == expected


def test_simple_slash_date_T_with_hours_minutes():
    got = from_str_to_datetime("2000/10/01T12:02")
    expected = datetime.datetime(2000, 10, 1, 12, 2)
    assert got == expected


def test_simple_slash_date_T_with_hours_minutes_seconds():
    got = from_str_to_datetime("2000/10/01T12:02:14")
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14)
    assert got == expected


## YYYY-MM-DD
def test_simple_hyphen_date():
    got = from_str_to_datetime("2000-10-01")
    expected = datetime.datetime(2000, 10, 1, 0, 0)
    assert got == expected


### Separation : Space
def test_simple_hyphen_date_space_with_hours():
    got = from_str_to_datetime("2000-10-01 12")
    expected = datetime.datetime(2000, 10, 1, 12, 0)
    assert got == expected


def test_simple_hyphen_date_space_with_hours_minutes():
    got = from_str_to_datetime("2000-10-01 12:02")
    expected = datetime.datetime(2000, 10, 1, 12, 2)
    assert got == expected


def test_simple_hyphen_date_space_with_hours_minutes_seconds():
    got = from_str_to_datetime("2000-10-01 12:02:14")
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14)
    assert got == expected


def test_simple_hyphen_date_space_with_hours_minutes_seconds_milliseconds_short():
    got = from_str_to_datetime("2000-10-01 12:02:14.123")
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14, 123000)
    assert got == expected


def test_simple_hyphen_date_space_with_hours_minutes_seconds_milliseconds_long():
    got = from_str_to_datetime("2000-10-01 12:02:14.851541")
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14, 851541)
    assert got == expected


### Separation : T
def test_simple_hyphen_date_T_with_hours():
    got = from_str_to_datetime("2000-10-01T12")
    expected = datetime.datetime(2000, 10, 1, 12, 0)
    assert got == expected


def test_simple_hyphen_date_T_with_hours_minutes():
    got = from_str_to_datetime("2000-10-01T12:02")
    expected = datetime.datetime(2000, 10, 1, 12, 2)
    assert got == expected


def test_simple_hyphen_date_T_with_hours_minutes_seconds():
    got = from_str_to_datetime("2000-10-01T12:02:14")
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14)
    assert got == expected


def test_simple_hyphen_date_T_with_hours_minutes_seconds_milliseconds_short():
    got = from_str_to_datetime("2000-10-01T12:02:14.123")
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14, 123000)
    assert got == expected


def test_simple_hyphen_date_T_with_hours_minutes_seconds_milliseconds_long():
    got = from_str_to_datetime("2000-10-01T12:02:14.851541")
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14, 851541)
    assert got == expected


## Time zone
def test_simple_hyphen_date_T_with_hours_minutes_seconds_Z():
    got = from_str_to_datetime("2000-10-01T12:02:14Z")
    tz = datetime.timezone.utc
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14, tzinfo=tz)
    assert got == expected


def test_simple_hyphen_date_T_with_hours_Z():
    got = from_str_to_datetime("2000-10-01T12Z")
    tz = datetime.timezone.utc
    expected = datetime.datetime(2000, 10, 1, 12, 0, tzinfo=tz)
    assert got == expected


def test_simple_hyphen_date_T_with_hours_minutes_seconds_milliseconds_long_plus_6_colons():
    got = from_str_to_datetime("2000-10-01T12:02:14.851541+06:00")
    tz = datetime.timezone(datetime.timedelta(hours=6))
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14, 851541, tzinfo=tz)
    assert got == expected


def test_simple_hyphen_date_T_with_hours_minutes_seconds_milliseconds_long_plus_8():
    got = from_str_to_datetime("2000-10-01T12:02:14.851541+0800")
    tz = datetime.timezone(datetime.timedelta(hours=8))
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14, 851541, tzinfo=tz)
    assert got == expected


def test_simple_hyphen_date_T_with_hours_minutes_seconds_moins_3():
    got = from_str_to_datetime("2000-10-01T12:02:14-0300")
    tz = datetime.timezone(datetime.timedelta(hours=-3))
    expected = datetime.datetime(2000, 10, 1, 12, 2, 14, tzinfo=tz)
    assert got == expected


## Random examples
def test_simple_date_with_colons_in_timezone():
    got = from_str_to_datetime("2021-10-11T14:57:33+00:00")
    tz = datetime.timezone.utc
    expected = datetime.datetime(2021, 10, 11, 14, 57, 33, tzinfo=tz)
    assert got == expected


def test_simple_date_with_colons_hours_minutes_in_timezone():
    got = from_str_to_datetime("2021-10-11T14:57:33+01:23")
    tz = datetime.timezone(datetime.timedelta(hours=1, minutes=23))
    expected = datetime.datetime(2021, 10, 11, 14, 57, 33, tzinfo=tz)
    assert got == expected


def test_simple_date_in_HrFlow_tag():
    got = from_str_to_datetime("2021-10-13T10:57:38+0200")
    tz = datetime.timezone(datetime.timedelta(hours=2))
    expected = datetime.datetime(2021, 10, 13, 10, 57, 38, tzinfo=tz)
    assert got == expected


def test_simple_date_in_HrFlow():
    got = from_str_to_datetime("2021-10-01T12:59:05+0000")
    tz = datetime.timezone.utc
    expected = datetime.datetime(2021, 10, 1, 12, 59, 5, tzinfo=tz)
    assert got == expected


def test_wrong_date_with_letter():
    try:
        from_str_to_datetime("2021-1H-01T12:59:05+0000")
        assert False
    except DateFormatError:
        pass


def test_wrong_date_with_invalid_date():
    try:
        from_str_to_datetime("2021-10-32T12:59:05+0000")
        assert False
    except DateFormatError:
        pass


def test_wrong_date_with_invalid_format():
    try:
        from_str_to_datetime("2021-10sssss")
        assert False
    except DateFormatError:
        pass


def test_wrong_date_with_invalid_int():
    try:
        from_str_to_datetime("2021-ss-ss")
        assert False
    except DateFormatError:
        pass