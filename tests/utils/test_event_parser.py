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

def test_event_with_profile_in_listened_source():
    request = dict(profile=dict(key="abc", source=dict(key="efg")))
    event = EventParser(request=request)
    source_to_listen = ["xyz", "efg"]
    profile = event.get_profile(source_to_listen=source_to_listen)
    assert profile is not None
    assert isinstance(profile, Profile)
    assert profile.key == "abc"
    assert profile.source.key == "efg"

def test_event_with_profile_not_in_listened_source():
    request = dict(profile=dict(key="abc", source=dict(key="efg")))
    event = EventParser(request=request)
    source_to_listen = ["xyz", "hij"]
    profile = event.get_profile(source_to_listen=source_to_listen)
    assert profile is None