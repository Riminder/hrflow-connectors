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
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/document/parsing",
        status=200,
        json=generated_parsing_text_response,
    )

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