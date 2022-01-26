from hrflow_connectors import Monster

import requests
import pytest
from typing import Dict, Any


@pytest.fixture()
def monster_request() -> Dict[str, Any]:
    """
    Get credentials from a file in the root of the project `credentials.json` (to be defined)

    Returns:
        Dict[str, Any]: Credentials
    """

    url = (
        "https://riminder-documents"
        "-eu-2019-12.s3-eu-west-1.amazonaws.com/te"
        "ams/fc9d40fd60e679119130ea74ae1d34a3e22174f2/sourc"
        "es/8df6a1247b1a95e0b84f5226093ff2c58e60cdf1/profiles/64"
        "baf80a58f3c6e434d9517311fdfec5cf7c9996/parsing/resume.pdf"
    )

    r = requests.get(url)
    type(r.content)
    file_contents = list(r.content)

    return {
        "City": "Paris",
        "CountryCode": "FR",
        "EmailAddress": "clement.negre@hrflow.ai",
        "FileContents": file_contents,
        "FileExt": ".pdf",
        "FirstName": "Elmoustapha",
        "JobRefID": "Job common fields",
        "LastName": "EBNOU",
        "PhoneNumber": "+PhoneNumber",
        "ResumeValue": "fwj9euwqsmfuknqr",
        "State": "IDF",
        "VendorField": "This apply comes from a job on Monster",
        "WorkAuthorization": 1,
        "ZIPCode": "75008",
    }


def test_CatchProfileBaseAction(logger, hrflow_client, monster_request):
    response = Monster.catch_profile(
        hrflow_client=hrflow_client(),
        request=monster_request,
        source_key="8df6a1247b1a95e0b84f5226093ff2c58e60cdf1",
    )
    assert response.get("status_code") == 201
