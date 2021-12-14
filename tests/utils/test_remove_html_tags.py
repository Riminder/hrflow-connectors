from hrflow_connectors.utils.clean_text import remove_html_tags


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
