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


def is_valid_url(url):
    regex = r"^https?:\/\/(www\.)?[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}\/?[^\s]*$"
    return re.match(regex, url) is not None
