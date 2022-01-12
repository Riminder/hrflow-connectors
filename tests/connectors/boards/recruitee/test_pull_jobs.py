from hrflow_connectors.connectors.boards.recruitee import PullJobs


def test_PullJobs(logger, hrflow_client):
    action = PullJobs(
        subdomain='testhr',
        hrflow_client=hrflow_client("dev-demo"),
        board_key="c430a1fe111a4076605a8dee85448300bd40f890",
        hydrate_with_parsing=True,
    )
    action.execute()