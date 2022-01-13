from hrflow_connectors import Recruitee


def test_PullJobs(logger, hrflow_client):
    Recruitee.pull_jobs(
        subdomain='testhr',
        hrflow_client=hrflow_client("dev-demo"),
        board_key="c430a1fe111a4076605a8dee85448300bd40f890",
        hydrate_with_parsing=True,
    )