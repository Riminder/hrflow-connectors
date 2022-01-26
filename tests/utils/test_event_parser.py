from hrflow_connectors.utils.hrflow import EventParser, Profile, Job


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


def test_event_with_job():
    request = dict(job=dict(key="abc", board=dict(key="efg")))
    event = EventParser(request=request)
    job = event.get_job()
    assert job is not None
    assert isinstance(job, Job)
    assert job.key == "abc"
    assert job.board.key == "efg"


def test_event_without_job():
    request = dict(other=dict(key="abc", board=dict(key="efg")))
    event = EventParser(request=request)
    job = event.get_job()
    assert job is None


def test_event_with_job_in_listened_board():
    request = dict(job=dict(key="abc", board=dict(key="efg")))
    event = EventParser(request=request)
    board_to_listen = ["xyz", "efg"]
    job = event.get_job(board_to_listen=board_to_listen)
    assert job is not None
    assert isinstance(job, Job)
    assert job.key == "abc"
    assert job.board.key == "efg"


def test_event_with_job_not_in_listened_board():
    request = dict(job=dict(key="abc", board=dict(key="efg")))
    event = EventParser(request=request)
    board_to_listen = ["xyz", "hij"]
    job = event.get_job(board_to_listen=board_to_listen)
    assert job is None