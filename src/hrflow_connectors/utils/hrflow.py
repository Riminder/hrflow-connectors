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


def generate_workflow_response(status_code=201, **kwargs) -> Dict[str, Any]:
    """
    Generate CATCH workflow response

    Args:
        status_code (int, optional): status code like HTTP code. Defaults to 201.
        **kwargs: additional fields to add to the returned response

    Returns:
        Dict[str, Any]: Generated response
    """
    headers = {"Content-Type": "application/json"}
    response = dict(status_code=status_code, headers=headers)
    response.update(kwargs)
    return response
