from hrflow_connectors import Greenhouse


def test_PullJobsAcrion(logger, hrflow_client):
    Greenhouse.pull_jobs(
        board_token="vaulttec",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="f02c4d041ce04a4657e7aaa360fdeb3712aec130",
        hydrate_with_parsing=True,
    )