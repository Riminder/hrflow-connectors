from hrflow_connectors.core.action import Action
import pytest

@pytest.fixture
def generated_data_list():
    list_to_filter = []
    list_to_filter.append(dict(element1="value1", element2="value2"))
    list_to_filter.append(dict(element1="value1", element2="value1"))
    list_to_filter.append(dict(element1="value2", element2="value1"))
    list_to_filter.append(dict(element1="value2", element2="value2"))
    return list_to_filter

def test_apply_logics_single_filter(generated_data_list):
    def filter_element1_with_value1(element):
        return element.get("element1") == "value1"
    
    action = Action(logics=["filter_element1_with_value1"], global_scope=globals(), local_scope=locals())
    filtered_list = action.apply_logics(generated_data_list)

    assert len(filtered_list) == 2
    assert dict(element1="value1", element2="value2") in filtered_list
    assert dict(element1="value1", element2="value1") in filtered_list

def test_apply_logics_two_filter(generated_data_list):
    def filter_element1_with_value1(element):
        return element.get("element1") == "value1"
    
    def filter_element2_with_value1(element):
        return element.get("element2") == "value1"
    
    action = Action(logics=["filter_element1_with_value1", "filter_element2_with_value1"], global_scope=globals(), local_scope=locals())
    filtered_list = action.apply_logics(generated_data_list)

    assert len(filtered_list) == 1
    assert dict(element1="value1", element2="value1") in filtered_list

def test_apply_logics_single_filter_without_interaction(generated_data_list):
    def filter_nothing(element):
        return True
    
    action = Action(logics=["filter_nothing"], global_scope=globals(), local_scope=locals())
    filtered_list = action.apply_logics(generated_data_list)

    assert len(filtered_list) == 4
    assert dict(element1="value1", element2="value2") in filtered_list
    assert dict(element1="value1", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value1") in filtered_list
    assert dict(element1="value2", element2="value2") in filtered_list
