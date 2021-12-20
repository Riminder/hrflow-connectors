from hrflow_connectors.utils.hrflow import EventParser, Profile

def test_event_with_profile():
    request = dict(profile=dict(key="abc", source=dict(key="efg")))
    event = EventParser(request=request)
    profile = event.get_profile()
    assert profile is not None
    assert isinstance(profile, Profile)
    assert profile.key == "abc"
    assert profile.source.key == "efg"

def test_event_without_profile():
    request = dict(other=dict(key="abc", source=dict(key="efg")))
    event = EventParser(request=request)
    profile = event.get_profile()
    assert profile is None