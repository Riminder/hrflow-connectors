from hrflow_connectors.utils.hrflow import find_element_in_list

def test_find_element_in_list():
    element_list = [dict(n="a", v=2), dict(n="b", v=3)]
    assert find_element_in_list(element_list, n="a") == dict(n="a", v=2)
    assert find_element_in_list(element_list, n="b", v=3) == dict(n="b", v=3)
    assert find_element_in_list(element_list, v=3) == dict(n="b", v=3)
    assert find_element_in_list(element_list, v=42) == None
    assert find_element_in_list(element_list, n=3) == None
    assert find_element_in_list(element_list, n="b", v=42) == None
    assert find_element_in_list(element_list, n="b", v=2) == None
