from hrflow_connectors import Workable


def test_PullJobsAcrion(logger, hrflow_client):
    Workable.pull_jobs(
        subdomain="eurostar",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="12dc2abb0d952bfcbbc6259b76c0631754459787",
        hydrate_with_parsing=True,
    )