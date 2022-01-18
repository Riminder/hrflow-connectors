from hrflow_connectors.utils.hrflow import Job, Board


def test_job():
    job = Job(key="abc", board=Board(key="efg"))
    assert job.key == "abc"
    assert job.board.key == "efg"


def test_board():
    board = Board(key="efg")
    assert board.key == "efg"


def test_parse_job():
    job_dict = dict(key="abc", board=dict(key="efg"))
    job_obj = Job.parse_obj(job_dict)
    assert job_obj.key == "abc"
    assert job_obj.board.key == "efg"