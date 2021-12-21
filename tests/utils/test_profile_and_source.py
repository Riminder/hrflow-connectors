from hrflow_connectors.utils.hrflow import Profile, Source

def test_profile():
    profile = Profile(key="abc", source=Source(key="efg"))
    assert profile.key == "abc"
    assert profile.source.key == "efg"

def test_source():
    source = Source(key="efg")
    assert source.key == "efg"

def test_parse_profile():
    profile_dict = dict(key="abc", source=dict(key="efg"))
    profile_obj = Profile.parse_obj(profile_dict)
    assert profile_obj.key == "abc"
    assert profile_obj.source.key == "efg"