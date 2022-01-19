from hrflow_connectors import Ceridian


def test_PullJobs(logger, hrflow_client):
    Ceridian.pull_jobs(
        subdomain='ustest61-services',
        client_name_space = 'ddn',
        hrflow_client=hrflow_client("dev-demo"),
        board_key="f54e75c62da8f3273789e10a3c89d9877b72c66d",
        hydrate_with_parsing=True,
    )
