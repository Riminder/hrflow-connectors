from hrflow_connectors.utils.timedelta_converter import (
    from_str_to_timedelta,
    TimeDeltaFormatError,
)


def assert_datetime(datetime_string):
    str_in_timedelta = from_str_to_timedelta(datetime_string)
    got_string = str(str_in_timedelta)
    expected_string = datetime_string
    assert got_string == expected_string


def test_simple_timedelta_with_8_hours():
    assert_datetime("8:05:32")


def test_simple_timedelta_with_23_hours():
    assert_datetime("23:05:32")


def test_simple_timedelta_with_days_hours():
    assert_datetime("66 days, 8:05:56")


def test_simple_neg_timedelta_with_days_hours():
    assert_datetime("-66 days, 8:05:56")


def test_simple_timedelta_with_days_hours_millisecond():
    assert_datetime("-1803 days, 15:54:03.999990")


def test_simple_neg_timedelta_with_days_hours_millisecond():
    assert_datetime("-1804 days, 15:54:03.999990")


def test_simple_neg_timedelta_with_one_day_hours_millisecond():
    assert_datetime("-1 day, 15:54:03.999990")


def test_wrong_timedelta_with_days_hours_millisecond_space_missing():
    try:
        assert_datetime("-1803days, 15:54:03.999990")
        assert False
    except TimeDeltaFormatError:
        pass


def test_wrong_timedelta_with_days_hours_millisecond_with_letter():
    try:
        assert_datetime("-HH days, 15:54:03.999990")
        assert False
    except TimeDeltaFormatError:
        pass


def test_wrong_timedelta_with_days_hours_millisecond_with_plus():
    try:
        assert_datetime("+01 days, 15:54:03.999990")
        assert False
    except TimeDeltaFormatError:
        pass


def test_wrong_timedelta_with_days_hours_without_seconds():
    try:
        assert_datetime("1 days, 15:54")
        assert False
    except TimeDeltaFormatError:
        pass

def test_wrong_timedelta_with_invalid_int():
    try:
        assert_datetime("1 days, 15:54:ss")
        assert False
    except TimeDeltaFormatError:
        pass