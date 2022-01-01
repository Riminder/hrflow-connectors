from hrflow_connectors.connectors.boards.craigslist.actions import CraigslistFeed


def test_CraigslistJobs(logger, hrflow_client, webdriver_path):
    action = CraigslistFeed(
        executable_path=webdriver_path,
        subdomain="Paris",
        hrflow_client=hrflow_client("dev-demo"),
        board_key="fc197c369ed9a8d35041961b268076885b2ea6f2",
        hydrate_with_parsing=True,
    )
    action.execute()
