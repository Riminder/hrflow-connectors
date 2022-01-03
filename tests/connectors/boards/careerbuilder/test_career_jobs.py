from hrflow_connectors.connectors.boards.careerbuilder import GetAllJobs


def test_CareerJobs(logger, hrflow_client, webdriver_path):
    
    action = GetAllJobs(
        executable_path=webdriver_path,
        domain='fr',
        hrflow_client=hrflow_client("dev-demo"),
        maximum_page_num=1,
        job_search='Data Scientist',
        job_location='Paris',
        hydrate_with_parsing=True,
        board_key="4dda21ae8a3bd3817f0f98ee716dad590c4be87e",
        )
    action.execute()