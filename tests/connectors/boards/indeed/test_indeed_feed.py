from hrflow_connectors.connectors.boards.indeed import IndeedFeed


def test_IndeedFeed(logger, hrflow_client, webdriver_path):
    action = IndeedFeed(
        executable_path=webdriver_path,
        max_page=2,
        subdomain="fr",
        hrflow_client=hrflow_client("dev-demo"),
        job_search="Software Engineer",
        job_location="Paris",
        board_key="5865a71e45b94e29f7c1c97d71479ef2757df414",
        hydrate_with_parsing=True,
    )
    action.execute()
