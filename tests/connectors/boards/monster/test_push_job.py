import pytest

from hrflow_connectors.connectors.boards.monster.actions import PushJob
from hrflow_connectors.utils.hrflow import Job, Board


def test_PushJob(logger, hrflow_client, credentials):
    job = Job(
        key="230f841bc57774bde1ad67563d9f092a788364e0",
        board=Board(key="d31518949ed1f88ac61308670324f93bc0f9374d"),
    )
    action = PushJob(
        username=credentials["monster"]["job"]["username"],
        password=credentials["monster"]["job"]["password"],
        subdomain="gateway",
        board_key=job.board.key,
        hrflow_client=hrflow_client(),
        archive_deleted_jobs_from_stream=False,
        job=job,
    )
    response = action.execute()
    assert response.get("status_code") == 201