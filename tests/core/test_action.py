from hrflow_connectors.core.action import Action
import pytest
import requests
import responses


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
