from hrflow_connectors.core.action import (
    BaseAction,
    PullBaseAction,
    PushBaseAction,
    PullJobsBaseAction,
    PushProfileBaseAction,
    PushJobBaseAction,
    CatchProfileBaseAction,
)
from hrflow_connectors.utils.hrflow import Profile, Source, Job, Board
import pytest
import requests
import responses
from hrflow import Hrflow

##################
### BaseAction ###
##################


@pytest.fixture
def generated_data_list():
    list_to_filter = []
    list_to_filter.append(dict(element1="value1", element2="value2"))
    list_to_filter.append(dict(element1="value1", element2="value1"))
    list_to_filter.append(dict(element1="value2", element2="value1"))
    list_to_filter.append(dict(element1="value2", element2="value2"))
    return list_to_filter


def test_BaseAction_apply_logics_with_empty_logics_list(
    hrflow_client, generated_data_list
):
    action = BaseAction(hrflow_client=hrflow_client())
    filtered_list = action.apply_logics(generated_data_list)

    assert len(filtered_list) == 4
    assert dict(element1="value1", element2="value2") in filtered_list
    assert dict(element1="value1", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value2") in filtered_list


def test_BaseAction_apply_logics_single_filter(hrflow_client, generated_data_list):
    def filter_element1_with_value1(element):
        return element.get("element1") == "value1"

    action = BaseAction(
        hrflow_client=hrflow_client(),
        logics=["filter_element1_with_value1"],
        global_scope=globals(),
        local_scope=locals(),
    )
    filtered_list = list(action.apply_logics(generated_data_list))

    assert len(filtered_list) == 2
    assert dict(element1="value1", element2="value2") in filtered_list
    assert dict(element1="value1", element2="value1") in filtered_list


def test_BaseAction_apply_logics_two_filter(hrflow_client, generated_data_list):
    def filter_element1_with_value1(element):
        return element.get("element1") == "value1"

    def filter_element2_with_value1(element):
        return element.get("element2") == "value1"

    action = BaseAction(
        hrflow_client=hrflow_client(),
        logics=["filter_element1_with_value1", "filter_element2_with_value1"],
        global_scope=globals(),
        local_scope=locals(),
    )
    filtered_list = list(action.apply_logics(generated_data_list))

    assert len(filtered_list) == 1
    assert dict(element1="value1", element2="value1") in filtered_list


def test_BaseAction_apply_logics_single_filter_without_interaction(
    hrflow_client, generated_data_list
):
    def filter_nothing(element):
        return True

    action = BaseAction(
        hrflow_client=hrflow_client(),
        logics=["filter_nothing"],
        global_scope=globals(),
        local_scope=locals(),
    )
    filtered_list = list(action.apply_logics(generated_data_list))

    assert len(filtered_list) == 4
    assert dict(element1="value1", element2="value2") in filtered_list
    assert dict(element1="value1", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value2") in filtered_list


def test_BaseAction_extern_format_function(hrflow_client):
    def extern_format(data):
        return dict(c=data["a"], d=data["a"] + data["b"])

    action = BaseAction(
        hrflow_client=hrflow_client(),
        format_function_name="extern_format",
        global_scope=globals(),
        local_scope=locals(),
    )

    job_to_transform = dict(a="aaa", b="bbb", f="fff")
    transformed_job = action.format_switcher(job_to_transform)
    assert transformed_job == dict(c="aaa", d="aaabbb")


def test_BaseAction_default_format_without_extern_format_function(hrflow_client):
    action = BaseAction(
        hrflow_client=hrflow_client(),
        format_function_name=None,
    )
    job_to_transform = dict(a="aaa", b="bbb", f="fff")
    transformed_job = action.format(job_to_transform)
    assert transformed_job == dict(a="aaa", b="bbb", f="fff")


def test_BaseAction_overwritten_format_with_extern_format_function(hrflow_client):
    def extern_format(data):
        return dict(c=data["a"], d=data["a"] + data["b"])

    class TestAction(BaseAction):
        def format(self, data):
            return dict(f=data["f"], g=data["a"] + data["b"])

    action = TestAction(
        hrflow_client=hrflow_client(),
        format_function_name="extern_format",
        global_scope=globals(),
        local_scope=locals(),
    )

    job_to_transform = dict(a="aaa", b="bbb", f="fff")
    transformed_job = action.format_switcher(job_to_transform)
    assert transformed_job == dict(c="aaa", d="aaabbb")


@responses.activate
def test_BaseAction_connect_and_execute(hrflow_client, generated_data_list):
    # Build a connector from `generated_data_list` to `http://test.test/push`
    class TestConnectorAction(BaseAction):
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
    action = TestConnectorAction(hrflow_client=hrflow_client())
    action.execute()


######################
### PullBaseAction ###
######################


def test_PullBaseAction_execute(hrflow_client):
    class MyPullAction(PullBaseAction):
        def pull(self):
            return ["pullformat", "pulllogic"]

        def format(self, data):
            assert "pull" in data
            return data.replace("pull", "")

        def push(self, data):
            data_list = list(data)
            assert data_list == ["logic"]

    def my_logic(data):
        return not data == "format"

    action = MyPullAction(
        hrflow_client=hrflow_client,
        logics=["my_logic"],
        local_scope=locals(),
        global_scope=globals(),
    )
    action.execute()


######################
### PushBaseAction ###
######################


def test_PushBaseAction_execute(hrflow_client):
    class MyPushAction(PushBaseAction):
        def pull(self):
            return ["pullformat", "pulllogic"]

        def format(self, data):
            assert "pull" in data
            return data.replace("pull", "")

        def push(self, data):
            data_list = list(data)
            assert data_list == ["format"]

    def my_logic(data):
        return not data == "pulllogic"

    action = MyPushAction(
        hrflow_client=hrflow_client,
        logics=["my_logic"],
        local_scope=locals(),
        global_scope=globals(),
    )
    action.execute()


##########################
### PullJobsBaseAction ###
##########################


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
def test_PullJobsBaseAction_get_all_references_from_board(
    hrflow_client, generated_jobs
):
    # Generate pages of jobs
    page_1 = generated_jobs(page=1, jobs=30)
    page_2 = generated_jobs(page=2, jobs=29)

    # Generate responses return by Hrflow
    expected_page_1_params = dict(
        board_keys='["abc"]', limit="30", page="1", sort_by="created_at"
    )
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/jobs/searching",
        status=200,
        match=[responses.matchers.query_param_matcher(expected_page_1_params)],
        json=generate_hrflow_search_response(page_1, max_page=2),
    )

    expected_page_2_params = dict(
        board_keys='["abc"]', limit="30", page="2", sort_by="created_at"
    )
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/jobs/searching",
        status=200,
        match=[responses.matchers.query_param_matcher(expected_page_2_params)],
        json=generate_hrflow_search_response(page_2, max_page=2),
    )

    # Catch requests sent to Hrflow
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
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


@responses.activate
def test_PullJobsBaseAction_get_all_references_from_board_failure(
    hrflow_client, generated_jobs
):
    # Generate pages of jobs
    page_1 = generated_jobs(page=1, jobs=30)
    page_2 = generated_jobs(page=2, jobs=29)

    # Generate responses return by Hrflow
    expected_page_1_params = dict(
        board_keys='["abc"]', limit="30", page="1", sort_by="created_at"
    )
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/jobs/searching",
        status=400,
        match=[responses.matchers.query_param_matcher(expected_page_1_params)],
        json=dict(code=400, message="Test get_job_page failed"),
    )

    # Catch requests sent to Hrflow
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    try:
        all_reference_iter = action.get_all_references_from_board()
        all_reference_list = list(all_reference_iter)
        assert False
    except RuntimeError:
        pass


@responses.activate
def test_PullJobsBaseAction_get_all_references_from_board_and_less_job_returned(
    hrflow_client, generated_jobs
):
    # Generate pages of jobs
    page_1 = generated_jobs(page=1, jobs=30)

    # Generate responses return by Hrflow
    expected_page_1_params = dict(
        board_keys='["abc"]', limit="30", page="1", sort_by="created_at"
    )
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/jobs/searching",
        status=200,
        match=[responses.matchers.query_param_matcher(expected_page_1_params)],
        json=generate_hrflow_search_response(page_1, max_page=2),
    )

    # Empty job list
    expected_page_2_params = dict(
        board_keys='["abc"]', limit="30", page="2", sort_by="created_at"
    )
    page_2_response = dict(code=200, message="Success", data=dict(jobs=[]))
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/jobs/searching",
        status=200,
        match=[responses.matchers.query_param_matcher(expected_page_2_params)],
        json=page_2_response,
    )

    # Catch requests sent to Hrflow
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    all_reference_iter = action.get_all_references_from_board()
    all_reference_list = list(all_reference_iter)

    assert len(all_reference_list) == 30

    all_reference_list.sort()
    page_1_expected = ["{}-{}".format(i, 1) for i in range(30)]

    reference_expected = page_1_expected
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
def test_PullJobsBaseAction_hydrate_job_with_parsing(
    hrflow_client, generated_parsing_text_response
):
    # Catch request
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/document/parsing",
        status=200,
        json=generated_parsing_text_response,
    )

    # Build Action
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
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


@responses.activate
def test_PullJobsBaseAction_hydrate_job_with_parsing_failure(
    hrflow_client, generated_parsing_text_response
):
    # Catch request
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/document/parsing",
        status=400,
        json=dict(code=400, message="Test fail to parse"),
    )

    # Build Action
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    section = dict(name="s", title=None, description="i speak english")
    job = dict(reference="REF123", summary="I love Python", sections=[section])

    assert len(job.get("skills", [])) == 0
    assert len(job.get("language", [])) == 0

    try:
        action.hydrate_job_with_parsing(job)
        assert False
    except RuntimeError:
        pass


@responses.activate
def test_PullJobsBaseAction_hydrate_job_with_parsing_with_empty_summary_and_only_html(
    hrflow_client, generated_parsing_text_response
):
    # Catch request
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/document/parsing5",
        status=200,
        json=generated_parsing_text_response,
    )

    # Build Action
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    section = dict(name="s", title=None, description='<html attr="Python"></html>')
    job = dict(reference="REF123", sections=[section])

    assert "skills" not in job
    assert "language" not in job

    hydrated_job = action.hydrate_job_with_parsing(job)

    assert "skills" not in hydrated_job
    assert "language" not in hydrated_job
    assert hydrated_job == job


@pytest.fixture
def generate_indexing_get_response():
    def indexing_get_response_func(code, message, archived_at):
        job = dict(key="klm", archived_at=archived_at)
        response = dict(code=code, message=message, data=job)
        return response

    return indexing_get_response_func


@responses.activate
def test_PullJobsBaseAction_check_reference_in_board_for_job_ref_none(
    hrflow_client,
    generate_indexing_get_response,
):
    # Generate job
    job = dict(reference=None)

    # Build Action
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    check_response = action.check_reference_in_board(job)
    assert check_response


@responses.activate
def test_PullJobsBaseAction_check_reference_in_board_for_job_not_in_board(
    hrflow_client,
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
    expected_params = dict(board_key="abc", reference="REF1")
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing",
        status=400,
        match=[responses.matchers.query_param_matcher(expected_params)],
        json=generated_response,
    )

    # Build Action
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    check_response = action.check_reference_in_board(job)
    assert check_response


@responses.activate
def test_PullJobsBaseAction_check_reference_in_board_for_not_archived_job_in_board(
    hrflow_client,
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
    expected_params = dict(board_key="abc", reference="REF1")
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing",
        status=200,
        match=[responses.matchers.query_param_matcher(expected_params)],
        json=generated_response,
    )

    # Build Action
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    check_response = action.check_reference_in_board(job)
    assert not check_response


@responses.activate
def test_PullJobsBaseAction_check_reference_in_board_fail(
    hrflow_client, generate_indexing_get_response
):
    # Generate job
    job = dict(reference="REF1")

    # generated response
    message = "Fail"
    generated_response = generate_indexing_get_response(
        code=400, message=message, archived_at=None
    )

    # Catch request
    expected_params = dict(board_key="abc", reference="REF1")
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing",
        status=400,
        match=[responses.matchers.query_param_matcher(expected_params)],
        json=generated_response,
    )

    # Build Action
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    try:
        action.check_reference_in_board(job)
        assert False
    except RuntimeError:
        assert True


@responses.activate
def test_PullJobsBaseAction_check_reference_in_board_for_archived_job_without_parsing(
    hrflow_client,
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
    expected_params = dict(board_key="abc", reference="REF1")
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing",
        status=200,
        match=[responses.matchers.query_param_matcher(expected_params)],
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
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    check_response = action.check_reference_in_board(job)
    assert not check_response


@responses.activate
def test_PullJobsBaseAction_check_reference_in_board_for_archived_job_without_parsing_and_unarchiving_failed(
    hrflow_client,
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
    expected_params = dict(board_key="abc", reference="REF1")
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing",
        status=200,
        match=[responses.matchers.query_param_matcher(expected_params)],
        json=generated_response,
    )

    ## create a matcher to check if the JSON Body sent by the Connector is in the right shape and has the right values
    expected_body = dict(board_key="abc", reference="REF1", is_archive=False)
    match = [responses.matchers.json_params_matcher(expected_body)]
    responses.add(
        responses.PATCH,
        "https://api.hrflow.ai/v1/job/indexing/archive",
        status=400,
        match=match,
        json=dict(code=400, message="Test Unarchiving failed"),
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
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    check_response = action.check_reference_in_board(job)
    assert not check_response


@responses.activate
def test_PullJobsBaseAction_check_reference_in_board_for_archived_job_in_board_with_parsing(
    hrflow_client, generate_indexing_get_response, generated_parsing_text_response
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
    expected_params = dict(board_key="abc", reference="REF1")
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing",
        status=200,
        match=[responses.matchers.query_param_matcher(expected_params)],
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
    action = PullJobsBaseAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=True
    )
    check_response = action.check_reference_in_board(job)

    assert not check_response


def test_PullJobsBaseAction_get_all_references_from_stream(hrflow_client):
    jobs_in_stream = [dict(reference="REF1"), dict(reference="REF2")]
    references_in_stream = ["REF1", "REF2"]

    class TestPullJobsAction(PullJobsBaseAction):
        def pull(self):
            return jobs_in_stream

    # Build Action
    action = TestPullJobsAction(
        hrflow_client=hrflow_client(),
        board_key="abc",
        hydrate_with_parsing=False,
        archive_deleted_jobs_from_stream=True,
    )
    references_got = action.get_all_references_from_stream()
    assert list(references_got) == references_in_stream


@responses.activate
def test_PullJobsBaseAction_check_deletion_references_from_stream(hrflow_client):
    references_in_stream = ["REF1", "REF2"]
    references_in_board = ["REF1", "REF4"]

    class TestPullJobsAction(PullJobsBaseAction):
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
    action = TestPullJobsAction(
        hrflow_client=hrflow_client(), board_key="abc", hydrate_with_parsing=False
    )
    action.check_deletion_references_from_stream()


def test_PullJobsBaseAction_execute_with_archiving_and_parsing(hrflow_client):
    class MyPullJobsAction(PullJobsBaseAction):
        def pull(self):
            return ["pullformat", "pulllogic", "pullcheckref"]

        def format(self, data):
            assert "pull" in data
            return data.replace("pull", "")

        def check_deletion_references_from_stream(self):
            assert True

        def check_reference_in_board(self, data):
            return "checkref" != data

        def hydrate_job_with_parsing(self, data):
            return data + "_after_parsing"

        def push(self, data):
            data_list = list(data)
            assert data_list == ["logic_after_parsing"]

    def my_logic(data):
        return not data == "format"

    action = MyPullJobsAction(
        hrflow_client=hrflow_client,
        board_key="abc",
        hydrate_with_parsing=True,
        logics=["my_logic"],
        local_scope=locals(),
        global_scope=globals(),
    )
    action.execute()


def test_PullJobsBaseAction_execute_with_archiving_without_parsing(hrflow_client):
    class MyPullJobsAction(PullJobsBaseAction):
        def pull(self):
            return ["pullformat", "pulllogic", "pullcheckref"]

        def format(self, data):
            assert "pull" in data
            return data.replace("pull", "")

        def check_deletion_references_from_stream(self):
            assert True

        def check_reference_in_board(self, data):
            return "checkref" != data

        def hydrate_job_with_parsing(self, data):
            return data + "_after_parsing"

        def push(self, data):
            data_list = list(data)
            assert data_list == ["logic"]

    def my_logic(data):
        return not data == "format"

    action = MyPullJobsAction(
        hrflow_client=hrflow_client,
        board_key="abc",
        hydrate_with_parsing=False,
        logics=["my_logic"],
        local_scope=locals(),
        global_scope=globals(),
    )
    action.execute()


def test_PullJobsBaseAction_execute_without_archiving_and_parsing(hrflow_client):
    class MyPullJobsAction(PullJobsBaseAction):
        def pull(self):
            return ["pullformat", "pulllogic", "pullcheckref"]

        def format(self, data):
            assert "pull" in data
            return data.replace("pull", "")

        def check_deletion_references_from_stream(self):
            assert False

        def check_reference_in_board(self, data):
            return "checkref" != data

        def hydrate_job_with_parsing(self, data):
            return data + "_after_parsing"

        def push(self, data):
            data_list = list(data)
            assert data_list == ["logic"]

    def my_logic(data):
        return not data == "format"

    action = MyPullJobsAction(
        hrflow_client=hrflow_client,
        board_key="abc",
        hydrate_with_parsing=False,
        archive_deleted_jobs_from_stream=False,
        logics=["my_logic"],
        local_scope=locals(),
        global_scope=globals(),
    )
    action.execute()


@responses.activate
def test_PullJobsBaseAction_push_success(hrflow_client):
    # Mock requests and check data sent
    job = dict(key="efg", reference="REF123")
    expected_body = dict(board_key="abc", **job)
    returned_value = dict(code=200, data=dict(key="efg"))
    match = [responses.matchers.json_params_matcher(expected_body)]
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/job/indexing",
        status=200,
        match=match,
        json=returned_value,
    )

    # Pull data
    action = PullJobsBaseAction(board_key="abc", hrflow_client=hrflow_client())
    action.push([job])


@responses.activate
def test_PullJobsBaseAction_push_failure(hrflow_client):
    # Mock requests and check data sent
    job = dict(key="efg", reference="REF123")
    expected_body = dict(board_key="abc", **job)
    returned_value = dict(code=400, message="Test")
    match = [responses.matchers.json_params_matcher(expected_body)]
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/job/indexing",
        status=400,
        match=match,
        json=returned_value,
    )

    # Pull data
    action = PullJobsBaseAction(board_key="abc", hrflow_client=hrflow_client())
    try:
        action.push([job])
        assert False
    except RuntimeError:
        pass


#############################
### PushProfileBaseAction ###
#############################


@responses.activate
def test_PushProfileBaseAction_pull_success(hrflow_client):
    # Mock requests and check data sent
    expected_params = dict(source_key="abc", key="efg")
    returned_value = dict(code=200, data=dict(key="efg"))
    match = [responses.matchers.query_param_matcher(expected_params)]
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/profile/indexing",
        status=200,
        match=match,
        json=returned_value,
    )

    # Pull data
    profile = Profile(key="efg", source=Source(key="abc"))
    action = PushProfileBaseAction(hrflow_client=hrflow_client(), profile=profile)
    profile_list_got = action.pull()

    # Check returned value
    assert len(profile_list_got) == 1

    profile_got = profile_list_got[0]
    assert profile_got["key"] == "efg"


@responses.activate
def test_PushProfileBaseAction_pull_failure(hrflow_client):
    # Mock requests and check data sent
    expected_params = dict(source_key="abc", key="efg")
    returned_value = dict(code=400, message="Test", data=dict())
    match = [responses.matchers.query_param_matcher(expected_params)]
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/profile/indexing",
        status=400,
        match=match,
        json=returned_value,
    )

    # Pull data
    profile = Profile(key="efg", source=Source(key="abc"))
    action = PushProfileBaseAction(hrflow_client=hrflow_client(), profile=profile)

    try:
        action.pull()
        assert False
    except RuntimeError:
        pass


@responses.activate
def test_PushProfileBaseAction_execute(hrflow_client):
    profile = Profile(key="efg", source=Source(key="abc"))

    # Mock the `pull` & `push` method to do nothing
    class MyPushProfileAction(PushProfileBaseAction):
        def pull(self):
            return []

        def push(self, data):
            return

    action = MyPushProfileAction(hrflow_client=hrflow_client(), profile=profile)

    workflow_response = action.execute()

    assert workflow_response["status_code"] == 201
    assert workflow_response["message"] == "Profile successfully pushed"


##########################
### PushJobeBaseAction ###
##########################


@responses.activate
def test_PushJobBaseAction_execute(hrflow_client):
    job = Job(key="efg", board=Board(key="abc"))

    # Mock the `pull` & `push` method to do nothing
    class MyPushJobAction(PushJobBaseAction):
        def pull(self):
            return ["pullformat", "pulllogic"]

        def format(self, data):
            assert "pull" in data
            return data.replace("pull", "")

        def push(self, data):
            data_list = list(data)
            assert data_list == ["format", "logic"]

    action = MyPushJobAction(hrflow_client=hrflow_client(), job=job)

    workflow_response = action.execute()

    assert workflow_response["status_code"] == 201
    assert workflow_response["message"] == "Profile successfully pushed"


@responses.activate
def test_PushJobBaseAction_pull_success(hrflow_client):
    # Mock requests and check data sent
    expected_params = dict(board_key="abc", key="efg")
    returned_value = dict(code=200, data=dict(key="efg"))
    match = [responses.matchers.query_param_matcher(expected_params)]
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing",
        status=200,
        match=match,
        json=returned_value,
    )

    # Pull data
    job = Job(key="efg", board=Board(key="abc"))
    action = PushJobBaseAction(hrflow_client=hrflow_client(), job=job)
    profile_list_got = action.pull()

    # Check returned value
    assert len(profile_list_got) == 1

    profile_got = profile_list_got[0]
    assert profile_got["key"] == "efg"


@responses.activate
def test_PushJobBaseAction_pull_failure(hrflow_client):
    # Mock requests and check data sent
    expected_params = dict(board_key="abc", key="efg")
    returned_value = dict(code=400, message="Test", data=dict())
    match = [responses.matchers.query_param_matcher(expected_params)]
    responses.add(
        responses.GET,
        "https://api.hrflow.ai/v1/job/indexing",
        status=400,
        match=match,
        json=returned_value,
    )

    # Pull data
    job = Job(key="efg", board=Board(key="abc"))
    action = PushJobBaseAction(hrflow_client=hrflow_client(), job=job)

    try:
        action.pull()
        assert False
    except RuntimeError:
        pass


##############################
### CatchProfileBaseAction ###
##############################


@responses.activate
def test_CatchProfileBaseAction_execute(hrflow_client):
    request = {
        "City": "xxx",
        "CountryCode": "x",
        "EmailAddress": "xxx",
        "FileContents": "xxxx",
        "FileExt": ".xx",
        "FirstName": "xxxxx",
        "JobRefID": "xxxx",
        "LastName": "xx",
        "PhoneNumber": "+xxxx",
        "ResumeValue": "x",
        "State": "x",
        "VendorField": "xxx xx xxxxx xx",
        "WorkAuthorization": 1,
        "ZIPCode": "xxxxxx",
    }

    # Mock the `pull` & `push` method to do nothing
    class MyCatchProfileAction(CatchProfileBaseAction):
        def format(self, data):
            assert data.get("City") == "xxx"
            data["ZIPCode"] = "X"
            return data

        def push(self, data):
            assert data["ZIPCode"] == "X"

    action = MyCatchProfileAction(
        hrflow_client=hrflow_client(),
        request=request,
        source_key="d31518949ed1f88ac61308670324f93bc0f9374d",
    )

    workflow_response = action.execute()

    assert workflow_response["status_code"] == 201
    assert workflow_response["message"] == "Profile successfully pushed"


@responses.activate
def test_CatchProfileBaseAction_push_success(hrflow_client):
    # Mock requests and check data sent
    returned_value = dict(code=200, message="ok", data=[])
    data_expected = dict(
        source_key="abc",
        labels="[]",
        tags="[]",
        metadatas="[]",
        sync_parsing=0,
        sync_parsing_indexing=1,
        webhook_parsing_sending=0,
    )

    files_expected = dict(file=b"base64")
    match = [
        responses.matchers.multipart_matcher(files=files_expected, data=data_expected)
    ]
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/profile/parsing/file",
        status=200,
        match=match,
        json=returned_value,
    )

    # Push data
    action = CatchProfileBaseAction(
        hrflow_client=hrflow_client(), source_key="abc", request=dict()
    )
    action.push(dict(source_key="abc", profile_file="base64"))


@responses.activate
def test_CatchProfileBaseAction_push_success(hrflow_client):
    # Mock requests and check data sent
    returned_value = dict(code=400, message="Test", data=[])
    data_expected = dict(
        source_key="abc",
        labels="[]",
        tags="[]",
        metadatas="[]",
        sync_parsing=0,
        sync_parsing_indexing=1,
        webhook_parsing_sending=0,
    )

    files_expected = dict(file=b"base64")
    match = [
        responses.matchers.multipart_matcher(files=files_expected, data=data_expected)
    ]
    responses.add(
        responses.POST,
        "https://api.hrflow.ai/v1/profile/parsing/file",
        status=400,
        match=match,
        json=returned_value,
    )

    # Push data
    action = CatchProfileBaseAction(
        hrflow_client=hrflow_client(), source_key="abc", request=dict()
    )
    try:
        action.push(dict(source_key="abc", profile_file="base64"))
        assert False
    except RuntimeError:
        pass