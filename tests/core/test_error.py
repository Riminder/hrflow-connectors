import requests
import responses

from hrflow_connectors.core.error import PullError, PushError, HrflowError


@responses.activate
def test_PullError():
    # Generate request response
    responses.add(responses.GET, "http://test.test", body="server error", status=401)
    response = requests.get("http://test.test")

    # Expected error output
    expected_output = "Failed to pull\n"
    expected_output += "Message: `My message`\n"
    expected_output += "Status code: `401`\n"
    expected_output += "Server response: `b'server error'`"

    # Test error
    try:
        raise PullError(response, Message="My message")
    except PullError as e:
        assert e.__str__() == expected_output


@responses.activate
def test_PushError():
    # Generate request response
    responses.add(responses.GET, "http://test.test", body="server error", status=401)
    response = requests.get("http://test.test")

    # Expected error output
    expected_output = "Failed to push\n"
    expected_output += "Message: `My message`\n"
    expected_output += "Status code: `401`\n"
    expected_output += "Server response: `b'server error'`"

    # Test error
    try:
        raise PushError(response, Message="My message")
    except PushError as e:
        assert e.__str__() == expected_output


def test_HrflowError():
    # Create Hrflow response
    response = dict(code=500, message="Internal error", data=[])

    # Expected error output
    expected_output = "Failed to communicate with Hrflow\n"
    expected_output += "Message: `My message`\n"
    expected_output += "Status code: `500`\n"
    expected_output += "Hrflow message: `Internal error`"

    # Test error
    try:
        raise HrflowError(
            response, title="Failed to communicate with Hrflow", Message="My message"
        )
    except HrflowError as e:
        assert e.__str__() == expected_output