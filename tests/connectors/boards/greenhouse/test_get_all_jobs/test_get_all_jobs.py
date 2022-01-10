from hrflow_connectors.connectors.boards.greenhouse import GreenhousePullJobsAction


def test_GetAllJobs(logger, hrflow_client):
    action = GreenhousePullJobsAction(
        board_token='vaulttec',
        hrflow_client=hrflow_client("dev-demo"),
        board_key="61abdf5e11a8b75f2f5d1789339ee3638d4c6197",
        hydrate_with_parsing=True,
    )
    action.execute()