from hrflow_connectors.utils.clean_text import remove_html_tags
from hrflow_connectors.utils.hrflow import find_element_in_list

def test_remove_html_tag_with_br():
    html_content = "Hello <br>World !"
    output = remove_html_tags(html_content)
    expected = "Hello World !"
    assert output == expected


def test_remove_html_tag_without_tag_with_single_less_than_sign():
    html_content = "Hello < World !"
    output = remove_html_tags(html_content)
    expected = "Hello < World !"
    assert output == expected


def test_remove_html_tag_without_tag_with_multiple_less_than_signs():
    html_content = "Hello <<< World !"
    output = remove_html_tags(html_content)
    expected = "Hello <<< World !"
    assert output == expected


def test_remove_html_tag_with_div():
    html_content = 'Hello<div attr="One Two" attr2="Three"> World</div> !'
    output = remove_html_tags(html_content)
    expected = "Hello World !"
    assert output == expected



def test_find_element_in_list():
    element_list = [dict(n="a", v=2), dict(n="b", v=3)]
    assert find_element_in_list(element_list, n="a") == dict(n="a", v=2)
    assert find_element_in_list(element_list, n="b", v=3) == dict(n="b", v=3)
    assert find_element_in_list(element_list, v=3) == dict(n="b", v=3)
    assert find_element_in_list(element_list, v=42) == None
    assert find_element_in_list(element_list, n=3) == None
    assert find_element_in_list(element_list, n="b", v=42) == None
    assert find_element_in_list(element_list, n="b", v=2) == None