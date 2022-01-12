from hrflow_connectors import Greenhouse


def test_PullJobsAcrion(logger, hrflow_client):
    Greenhouse.pull_jobs(
        board_token="vaulttec",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="61abdf5e11a8b75f2f5d1789339ee3638d4c6197",
        hydrate_with_parsing=True,
    )