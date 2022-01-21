import requests
import responses

from hrflow_connectors.core.error import PullError, PushError


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