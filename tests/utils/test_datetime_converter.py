import datetime
from hrflow_connectors.utils.datetime_converter import (
    from_str_to_datetime,
    DateFormatError,
)
import pandas as pd


def assert_datetime(datetime_string):
    got = from_str_to_datetime(datetime_string)
    expected = pd.to_datetime(datetime_string).to_pydatetime()
    assert got == expected


## YYYY/MM/DD
def test_simple_slash_date():
    assert_datetime("2000/10/01")


### Separation : Space
def test_simple_slash_date_space_with_hours():
    assert_datetime("2000/10/01 12")


def test_simple_slash_date_space_with_hours_minutes():
    assert_datetime("2000/10/01 12:02")


def test_simple_slash_date_space_with_hours_minutes_seconds():
    assert_datetime("2000/10/01 12:02:14")


### Separation : T
def test_simple_slash_date_T_with_hours():
    assert_datetime("2000/10/01T12")


def test_simple_slash_date_T_with_hours_minutes():
    assert_datetime("2000/10/01T12:02")


def test_simple_slash_date_T_with_hours_minutes_seconds():
    assert_datetime("2000/10/01T12:02:14")


## YYYY-MM-DD
def test_simple_hyphen_date():
    assert_datetime("2000-10-01")


### Separation : Space
def test_simple_hyphen_date_space_with_hours():
    assert_datetime("2000-10-01 12")


def test_simple_hyphen_date_space_with_hours_minutes():
    assert_datetime("2000-10-01 12:02")


def test_simple_hyphen_date_space_with_hours_minutes_seconds():
    assert_datetime("2000-10-01 12:02:14")


def test_simple_hyphen_date_space_with_hours_minutes_seconds_milliseconds_short():
    assert_datetime("2000-10-01 12:02:14.123")


def test_simple_hyphen_date_space_with_hours_minutes_seconds_milliseconds_long():
    assert_datetime("2000-10-01 12:02:14.851541")


### Separation : T
def test_simple_hyphen_date_T_with_hours():
    assert_datetime("2000-10-01T12")


def test_simple_hyphen_date_T_with_hours_minutes():
    assert_datetime("2000-10-01T12:02")


def test_simple_hyphen_date_T_with_hours_minutes_seconds():
    assert_datetime("2000-10-01T12:02:14")


def test_simple_hyphen_date_T_with_hours_minutes_seconds_milliseconds_short():
    assert_datetime("2000-10-01T12:02:14.123")


def test_simple_hyphen_date_T_with_hours_minutes_seconds_milliseconds_long():
    assert_datetime("2000-10-01T12:02:14.851541")


## Time zone
def test_simple_hyphen_date_T_with_hours_minutes_seconds_Z():
    assert_datetime("2000-10-01T12:02:14Z")


def test_simple_hyphen_date_T_with_hours_Z():
    assert_datetime("2000-10-01T12Z")


def test_simple_hyphen_date_T_with_hours_minutes_seconds_milliseconds_long_plus_6_colons():
    assert_datetime("2000-10-01T12:02:14.851541+06:00")


def test_simple_hyphen_date_T_with_hours_minutes_seconds_milliseconds_long_plus_8():
    assert_datetime("2000-10-01T12:02:14.851541+0800")


def test_simple_hyphen_date_T_with_hours_minutes_seconds_moins_3():
    assert_datetime("2000-10-01T12:02:14-0300")


def test_simple_hyphen_date_space_with_hours():
    assert_datetime("2000-10-01 12")


## Random examples


def test_simple_date_in_DR():
    assert_datetime("2021-10-11T14:57:33+00:00")


def test_simple_date_in_HrFlow_tag():
    assert_datetime("2021-10-13T10:57:38+0200")


def test_simple_date_in_HrFlow():
    assert_datetime("2021-10-01T12:59:05+0000")


def test_wrong_date_with_letter():
    try:
        assert_datetime("2021-1H-01T12:59:05+0000")
    except DateFormatError:
        pass


def test_wrong_date_with_invalid_date():
    try:
        assert_datetime("2021-10-32T12:59:05+0000")
    except DateFormatError:
        pass


def test_wrong_date_with_invalid_format():
    try:
        assert_datetime("2021-10sssss")
    except DateFormatError:
        pass