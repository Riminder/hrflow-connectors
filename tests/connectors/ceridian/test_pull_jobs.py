from hrflow_connectors import Ceridian


def test_PullJobs(logger, hrflow_client):
    Ceridian.pull_jobs(
        subdomain='ustest61-services',
        client_name_space = 'ddn',
        hrflow_client=hrflow_client("dev-demo"),
        board_key="3bc89d440036641fedb04349e8e9767d77fa6830",
        hydrate_with_parsing=True,
    )
