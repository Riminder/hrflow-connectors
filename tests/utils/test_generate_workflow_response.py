from hrflow_connectors.utils.hrflow import generate_workflow_response


def test_generate_workflow_response_201_without_fields():
    response = generate_workflow_response(status_code=201)
    assert len(response) == 2
    assert response.get("status_code") == 201


def test_generate_workflow_response_400_without_fields():
    response = generate_workflow_response(status_code=400)
    assert len(response) == 2
    assert response.get("status_code") == 400


def test_generate_workflow_response_400_with_single_field():
    response = generate_workflow_response(status_code=400, field1="value1")
    assert len(response) == 3
    assert response.get("status_code") == 400
    assert response.get("field1") == "value1"


def test_generate_workflow_response_400_with_multi_fields():
    response = generate_workflow_response(
        status_code=400, field1="value1", field2="value2"
    )
    assert len(response) == 4
    assert response.get("status_code") == 400
    assert response.get("field1") == "value1"
    assert response.get("field2") == "value2"
