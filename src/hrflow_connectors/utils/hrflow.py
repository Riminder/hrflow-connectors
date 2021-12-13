from typing import Dict, Any, List, Optional


def find_element_in_list(
    element_list: List[Dict[str, Any]], **fields
) -> Optional[Dict[str, Any]]:
    """
    Find element with some fields in list of elements

    >>> element_list = [dict(n="a", v=2), dict(n="b", v=3)]
    >>> find_element_in_list(element_list, n="a")
    ... dict(n="a", v=2)
    >>> find_element_in_list(element_list, n="b", v=3)
    ... dict(n="b", v=3)
    >>> find_element_in_list(element_list, v=42)
    ... None

    Args:
        element_list (List[Dict[str, Any]]): list of element with differents fields

    Returns:
        Dict[str, Any]: return element. Otherwise, return None.
    """
    for element in element_list:
        if element.items() >= fields.items():
            return element
    return None