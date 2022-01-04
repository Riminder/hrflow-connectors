
from hrflow_connectors.connectors.boards.greenhouse import GetAllJobs


def test_GetAllJobs(logger, hrflow_client):
    action = GetAllJobs(
        board_token='vaulttec',
        hrflow_client=hrflow_client("dev-demo"),
        board_key="fc48d01539cde9e8a8ca17238a61c16826837729",
        hydrate_with_parsing=True,
    )
    action.execute()