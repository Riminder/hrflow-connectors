from hrflow_connectors import Recruitee


def test_PullJobs(logger, hrflow_client):
    Recruitee.pull_jobs(
        subdomain='testhr',
        hrflow_client=hrflow_client("dev-demo"),
        board_key="243ed7c3fef55e1bacee3ba71b6d589e2b21079a",
        hydrate_with_parsing=True,
    )