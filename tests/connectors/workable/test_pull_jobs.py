from hrflow_connectors import Workable


def test_PullJobsAcrion(logger, hrflow_client):
    Workable.pull_jobs(
        subdomain="eurostar",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="0d9a50e90e529e43394cf84b2f0666432551980b",
        hydrate_with_parsing=True,
    )