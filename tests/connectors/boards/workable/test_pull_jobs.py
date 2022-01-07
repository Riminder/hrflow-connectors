from hrflow_connectors.connectors.boards.workable import PullJobs


def test_PullJobs(logger, hrflow_client):
    action = PullJobs(
        subdomain='eurostar',
        hrflow_client=hrflow_client("dev-demo"),
        board_key="12dc2abb0d952bfcbbc6259b76c0631754459787",
        hydrate_with_parsing=True,
    )
    action.execute()