import re


def remove_html_tags(text: str) -> str:
    """
    Remove all HTML tags in a string
    Args:
        text (str): text to clean
    Returns:
        str: cleaned text (without HTML tags)
    """
    return re.sub("<[^<]+?>", "", text)
