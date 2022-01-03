from hrflow_connectors.core.action import Action, BoardAction
import pytest
import requests
import responses
from hrflow import Hrflow


@pytest.fixture
def generated_data_list():
    list_to_filter = []
    list_to_filter.append(dict(element1="value1", element2="value2"))
    list_to_filter.append(dict(element1="value1", element2="value1"))
    list_to_filter.append(dict(element1="value2", element2="value1"))
    list_to_filter.append(dict(element1="value2", element2="value2"))
    return list_to_filter


def test_apply_logics_with_empty_logics_list(generated_data_list):
    action = Action()
    filtered_list = action.apply_logics(generated_data_list)

    assert len(filtered_list) == 4
    assert dict(element1="value1", element2="value2") in filtered_list
    assert dict(element1="value1", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value2") in filtered_list


def test_apply_logics_single_filter(generated_data_list):
    def filter_element1_with_value1(element):
        return element.get("element1") == "value1"

    action = Action(
        logics=["filter_element1_with_value1"],
        global_scope=globals(),
        local_scope=locals(),
    )
    filtered_list = list(action.apply_logics(generated_data_list))

    assert len(filtered_list) == 2
    assert dict(element1="value1", element2="value2") in filtered_list
    assert dict(element1="value1", element2="value1") in filtered_list


def test_apply_logics_two_filter(generated_data_list):
    def filter_element1_with_value1(element):
        return element.get("element1") == "value1"

    def filter_element2_with_value1(element):
        return element.get("element2") == "value1"

    action = Action(
        logics=["filter_element1_with_value1", "filter_element2_with_value1"],
        global_scope=globals(),
        local_scope=locals(),
    )
    filtered_list = list(action.apply_logics(generated_data_list))

    assert len(filtered_list) == 1
    assert dict(element1="value1", element2="value1") in filtered_list


def test_apply_logics_single_filter_without_interaction(generated_data_list):
    def filter_nothing(element):
        return True

    action = Action(
        logics=["filter_nothing"], global_scope=globals(), local_scope=locals()
    )
    filtered_list = list(action.apply_logics(generated_data_list))

    assert len(filtered_list) == 4
    assert dict(element1="value1", element2="value2") in filtered_list
    assert dict(element1="value1", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value2") in filtered_list


def test_extern_format_function():
    def extern_format(data):
        return dict(c=data["a"], d=data["a"] + data["b"])

    action = Action(
        format_function_name="extern_format",
        global_scope=globals(),
        local_scope=locals(),
    )

    job_to_transform = dict(a="aaa", b="bbb", f="fff")
    transformed_job = action.format_switcher(job_to_transform)
    assert transformed_job == dict(c="aaa", d="aaabbb")


def test_default_format_without_extern_format_function():
    action = Action(
        format_function_name=None,
    )
    job_to_transform = dict(a="aaa", b="bbb", f="fff")
    transformed_job = action.format(job_to_transform)
    assert transformed_job == dict(a="aaa", b="bbb", f="fff")


def test_overwritten_format_with_extern_format_function():
    def extern_format(data):
        return dict(c=data["a"], d=data["a"] + data["b"])

    class TestAction(Action):
        def format(self, data):
            return dict(f=data["f"], g=data["a"] + data["b"])

    action = TestAction(
        format_function_name="extern_format",
        global_scope=globals(),
        local_scope=locals(),
    )

    job_to_transform = dict(a="aaa", b="bbb", f="fff")
    transformed_job = action.format_switcher(job_to_transform)
    assert transformed_job == dict(c="aaa", d="aaabbb")


@responses.activate
def test_Action_connect_and_execute(generated_data_list):
    # Build a connector from `generated_data_list` to `http://test.test/push`
    class TestConnectorAction(Action):
        def pull(self):
            return generated_data_list

        def format(self, data):
            element1 = data.get("element1")
            element2 = data.get("element2")
            adapted_data = dict(
                element3=element1, element4="{}+{}".format(element1, element2)
            )
            return adapted_data

        def push(self, data):
            requests.post("http://test.test/push", json=dict(data=list(data)))

    # Expected Output
    expected_output = []
    expected_output.append(dict(element3="value1", element4="value1+value2"))
    expected_output.append(dict(element3="value1", element4="value1+value1"))
    expected_output.append(dict(element3="value2", element4="value2+value1"))
    expected_output.append(dict(element3="value2", element4="value2+value2"))

    # Mock requests
    # create a matcher to check if the JSON Body sent by the Connector is in the right shape and has the right values
    match = [responses.matchers.json_params_matcher(dict(data=expected_output))]

    responses.add(
        responses.POST,
        "http://test.test/push",
        status=200,
        match=match,
    )

    # Exec action
    action = TestConnectorAction()
    action.execute()


@pytest.fixture
def generated_jobs():
    def generate_jobs_page(page: int, jobs=30):
        job_list = []
        for i in range(jobs):
            job_list.append(dict(reference="{}-{}".format(i, page)))
        return job_list

    return generate_jobs_page


def generate_hrflow_search_response(data, max_page=2):
    meta = dict(maxPage=max_page)
    response = dict(code=200, message="Success", meta=meta, data=dict(jobs=data))
    return response


@responses.activate
def test_BoardAction_get_all_references_from_board(generated_jobs):
    # Generate pages of jobs
    page_1 = generated_jobs(page=1, jobs=30)
    page_2 = generated_jobs(page=2, jobs=29)

    # Generate responses return by Hrflow
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/jobs/searching?board_keys=%5B%22abc%22%5D&limit=30&page=1&sort_by=created_at",
        status=200,
        json=generate_hrflow_search_response(page_1, max_page=2),
    )

    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/jobs/searching?board_keys=%5B%22abc%22%5D&limit=30&page=2&sort_by=created_at",
        status=200,
        json=generate_hrflow_search_response(page_2, max_page=2),
    )

    # Catch requests sent to Hrflow

    hrflow_client = Hrflow(api_user="", api_secret="")

    action = BoardAction(
        hrflow_client=hrflow_client, board_key="abc", hydrate_with_parsing=False
    )
    all_reference_iter = action.get_all_references_from_board()
    all_reference_list = list(all_reference_iter)

    assert len(all_reference_list) == 59

    all_reference_list.sort()
    page_1_expected = ["{}-{}".format(i, 1) for i in range(30)]
    page_2_expected = ["{}-{}".format(i, 2) for i in range(29)]

    reference_expected = page_1_expected + page_2_expected
    reference_expected.sort()

    assert all_reference_list == reference_expected


@pytest.fixture
def generated_parsing_text_response():
    text = "I love Python\ni speak english"
    ents = []
    ents.append(dict(start=7, end=13, label="HardSkill"))
    ents.append(dict(start=22, end=29, label="Language"))
    data = dict(ents=ents, text=text)
    response = dict(code=200, message="Success", data=data)
    return response


@responses.activate
def test_BoardAction_hydrate_job_with_parsing(generated_parsing_text_response):
    # Catch request
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/document/parsing",
        status=200,
        json=generated_parsing_text_response,
    )

    # Build Action
    hrflow_client = Hrflow(api_user="", api_secret="")

    action = BoardAction(
        hrflow_client=hrflow_client, board_key="abc", hydrate_with_parsing=False
    )
    section = dict(name="s", title=None, description="i speak english")
    job = dict(reference="REF123", summary="I love Python", sections=[section])

    assert len(job.get("skills", [])) == 0
    assert len(job.get("language", [])) == 0

    hydrated_job = action.hydrate_job_with_parsing(job)

    assert len(hydrated_job["skills"]) == 1
    assert hydrated_job["skills"][0] == dict(name="Python", type="hard", value=None)

    assert len(hydrated_job["languages"]) == 1
    assert hydrated_job["languages"][0] == dict(name="english", value=None)


@pytest.fixture
def generate_indexing_get_response():
    def indexing_get_response_func(code, message, archived_at):
        job = dict(key="klm", archived_at=archived_at)
        response = dict(code=code, message=message, data=job)
        return response

    return indexing_get_response_func


@responses.activate
def test_BoardAction_check_reference_in_board_for_job_not_in_board(
    generate_indexing_get_response,
):
    # Generate job
    job = dict(reference="REF1")

    # generated response
    message = "Invalid parameters. Unable to find object: job"
    generated_response = generate_indexing_get_response(
        code=400, message=message, archived_at=None
    )

    # Catch request
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing?board_key=abc&reference=REF1",
        status=400,
        json=generated_response,
    )

    # Build Action
    hrflow_client = Hrflow(api_user="", api_secret="")

    action = BoardAction(
        hrflow_client=hrflow_client, board_key="abc", hydrate_with_parsing=False
    )
    check_response = action.check_reference_in_board(job)
    assert check_response


@responses.activate
def test_BoardAction_check_reference_in_board_for_not_archived_job_in_board(
    generate_indexing_get_response,
):
    # Generate job
    job = dict(reference="REF1")

    # generated response
    message = "Job details"
    generated_response = generate_indexing_get_response(
        code=200, message=message, archived_at=None
    )

    # Catch request
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing?board_key=abc&reference=REF1",
        status=200,
        json=generated_response,
    )

    # Build Action
    hrflow_client = Hrflow(api_user="", api_secret="")

    action = BoardAction(
        hrflow_client=hrflow_client, board_key="abc", hydrate_with_parsing=False
    )
    check_response = action.check_reference_in_board(job)
    assert not check_response


@responses.activate
def test_BoardAction_check_reference_in_board_fail(generate_indexing_get_response):
    # Generate job
    job = dict(reference="REF1")

    # generated response
    message = "Fail"
    generated_response = generate_indexing_get_response(
        code=400, message=message, archived_at=None
    )

    # Catch request
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing?board_key=abc&reference=REF1",
        status=400,
        json=generated_response,
    )

    # Build Action
    hrflow_client = Hrflow(api_user="", api_secret="")

    action = BoardAction(
        hrflow_client=hrflow_client, board_key="abc", hydrate_with_parsing=False
    )
    try:
        action.check_reference_in_board(job)
        assert False
    except RuntimeError:
        assert True


@responses.activate
def test_BoardAction_check_reference_in_board_for_archived_job_in_board_without_parsing(
    generate_indexing_get_response,
):
    # Generate job
    job = dict(reference="REF1")

    # generated response
    message = "Job details"
    generated_response = generate_indexing_get_response(
        code=200, message=message, archived_at="2021-12-25T00:00:00"
    )

    # Catch request
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing?board_key=abc&reference=REF1",
        status=200,
        json=generated_response,
    )

    ## create a matcher to check if the JSON Body sent by the Connector is in the right shape and has the right values
    expected_body = dict(board_key="abc", reference="REF1", is_archive=False)
    match = [responses.matchers.json_params_matcher(expected_body)]
    responses.add(
        responses.PATCH,
        "https://api.hrflow.ai/v1/job/indexing/archive",
        status=200,
        match=match,
    )

    ## create a matcher to check if the JSON Body sent by the Connector is in the right shape and has the right values
    expected_body = dict(board_key="abc", key="klm", reference="REF1")
    match = [responses.matchers.json_params_matcher(expected_body)]
    responses.add(
        responses.PUT,
        "https://api.hrflow.ai/v1/job/indexing",
        status=200,
        match=match,
    )

    # Build Action
    hrflow_client = Hrflow(api_user="", api_secret="")

    action = BoardAction(
        hrflow_client=hrflow_client, board_key="abc", hydrate_with_parsing=False
    )
    check_response = action.check_reference_in_board(job)
    assert not check_response


@responses.activate
def test_BoardAction_check_reference_in_board_for_archived_job_in_board_with_parsing(
    generate_indexing_get_response, generated_parsing_text_response
):
    # Generate job
    section = dict(name="s", title=None, description="i speak english")
    job = dict(reference="REF1", summary="I love Python", sections=[section])

    assert len(job.get("skills", [])) == 0
    assert len(job.get("language", [])) == 0

    # generated response
    message = "Job details"
    generated_response = generate_indexing_get_response(
        code=200, message=message, archived_at="2021-12-25T00:00:00"
    )

    # Catch request
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing?board_key=abc&reference=REF1",
        status=200,
        json=generated_response,
    )

    ## create a matcher to check if the JSON Body sent by the Connector is in the right shape and has the right values
    expected_body = dict(board_key="abc", reference="REF1", is_archive=False)
    match = [responses.matchers.json_params_matcher(expected_body)]
    responses.add(
        responses.PATCH,
        "https://api.hrflow.ai/v1/job/indexing/archive",
        status=200,
        match=match,
    )

    # Catch parsing request
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/document/parsing",
        status=200,
        json=generated_parsing_text_response,
    )

    ## create a matcher to check if the JSON Body sent by the Connector is in the right shape and has the right values
    hydrated_job = dict(job)
    hydrated_job["skills"] = [dict(name="Python", type="hard", value=None)]
    hydrated_job["languages"] = [dict(name="english", value=None)]
    hydrated_job["certifications"] = []
    hydrated_job["courses"] = []
    hydrated_job["tasks"] = []

    expected_body = dict(board_key="abc", key="klm", **hydrated_job)
    match = [responses.matchers.json_params_matcher(expected_body)]
    responses.add(
        responses.PUT,
        "https://api.hrflow.ai/v1/job/indexing",
        status=200,
        match=match,
    )

    # Build Action
    hrflow_client = Hrflow(api_user="", api_secret="")

    action = BoardAction(
        hrflow_client=hrflow_client, board_key="abc", hydrate_with_parsing=True
    )
    check_response = action.check_reference_in_board(job)

    assert not check_response


def test_BoardAction_get_all_references_from_stream():
    jobs_in_stream = [dict(reference="REF1"), dict(reference="REF2")]
    references_in_stream = ["REF1", "REF2"]

    class TestBoardAction(BoardAction):
        def pull(self):
            return jobs_in_stream

    # Build Action
    hrflow_client = Hrflow(api_user="", api_secret="")

    action = TestBoardAction(
        hrflow_client=hrflow_client,
        board_key="abc",
        hydrate_with_parsing=False,
        archive_deleted_jobs_from_stream=True,
    )
    references_got = action.get_all_references_from_stream()
    assert list(references_got) == references_in_stream


@responses.activate
def test_BoardAction_check_deletion_references_from_stream():
    references_in_stream = ["REF1", "REF2"]
    references_in_board = ["REF1", "REF4"]

    class TestBoardAction(BoardAction):
        def get_all_references_from_board(self):
            return references_in_board

        def get_all_references_from_stream(self):
            return references_in_stream

    # Catch Archive request
    expected_body = dict(board_key="abc", reference="REF4", is_archive=True)
    match = [responses.matchers.json_params_matcher(expected_body)]
    responses.add(
        responses.PATCH,
        "https://api.hrflow.ai/v1/job/indexing/archive",
        status=200,
        match=match,
    )

    # Build Action
    hrflow_client = Hrflow(api_user="", api_secret="")

    action = TestBoardAction(
        hrflow_client=hrflow_client, board_key="abc", hydrate_with_parsing=False
    )
    action.check_deletion_references_from_stream()