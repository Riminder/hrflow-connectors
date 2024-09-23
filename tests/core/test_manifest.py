import json
import shutil
import tempfile
from pathlib import Path

import pytest
from PIL import Image

from hrflow_connectors import hrflow_connectors_manifest
from hrflow_connectors.core.connector import (
    MAX_LOGO_PIXEL,
    MAX_LOGO_SIZE_BYTES,
    MIN_LOGO_PIXEL,
    AmbiguousConnectorImportName,
    ConnectorImportNameNotFound,
)
from tests.core.test_connector import SmartLeadsF
from tests.core.utils import added_connectors, main_import_name_as


@pytest.fixture
def manifest_directory():
    path = Path(__file__).parent
    yield path
    manifest = path / "manifest.json"
    try:
        manifest.unlink()
    except FileNotFoundError:
        pass


def test_connector_manifest(test_connectors_directory):
    SmartLeads = SmartLeadsF()
    with added_connectors([("SmartLeads", SmartLeads)]):
        manifest = SmartLeads.manifest(test_connectors_directory)

    for action in manifest["actions"]:
        assert "from hrflow_connectors import SmartLeads" in action["workflow_code"]


def test_connector_manifest_works_with_parameterized_main_module_name(
    test_connectors_directory,
):
    parameterized_name = "third_party"

    SmartLeads = SmartLeadsF()
    with main_import_name_as(parameterized_name):
        # Should fail because by default add_connectors adds names to
        # hrflow_connectors default import name
        with pytest.raises(ModuleNotFoundError):
            with added_connectors([("SmartLeads", SmartLeads)]):
                SmartLeads.manifest(test_connectors_directory)

        with added_connectors(
            [("SmartLeads", SmartLeads)], parameterized_name, create_module=True
        ):
            manifest = SmartLeads.manifest(test_connectors_directory)

    for action in manifest["actions"]:
        assert f"from {parameterized_name} import SmartLeads" in action["workflow_code"]


def test_hrflow_connectors_manifest(manifest_directory, test_connectors_directory):
    manifest = Path(__file__).parent / "manifest.json"
    assert manifest.exists() is False

    connector = SmartLeadsF()
    target_connectors = [
        dict(name="SmartLeads", type="Other", subtype="smartleads"),
        dict(name="ATSConnector", type="ATS", subtype="atsconnector"),
        dict(
            name="AutomationConnector",
            type="Automation",
            subtype="automationconnector",
        ),
        dict(name="JobboardConnector", type="Job Board", subtype="jobboardconnector"),
        dict(name="WrongConnector", type=None, subtype="wrongconnector"),
    ]
    with added_connectors(
        [("SmartLeads", connector)],
    ):
        hrflow_connectors_manifest(
            connectors=[connector],
            target_connectors=target_connectors,
            directory_path=manifest_directory,
            connectors_directory=test_connectors_directory,
        )

    assert manifest.exists() is True
    assert len(json.loads(manifest.read_text())["connectors"]) == 4


def test_connector_manifest_fails_if_cannot_find_import_name(test_connectors_directory):
    SmartLeads = SmartLeadsF()
    with pytest.raises(ConnectorImportNameNotFound):
        SmartLeads.manifest(test_connectors_directory)


def test_connector_manifest_fails_if_connector_misconfigured(test_connectors_directory):
    SmartLeads = SmartLeadsF()
    with pytest.raises(AmbiguousConnectorImportName):
        with added_connectors([("SmartLeads", SmartLeads), ("Duplicated", SmartLeads)]):
            SmartLeads.manifest(test_connectors_directory)


def test_manifest_connector_directory_not_found(test_connectors_directory):
    SmartLeads = SmartLeadsF()
    SmartLeads.model.name = "SmartLeadsX"
    SmartLeads.model.subtype = "smartleadsx"
    with pytest.raises(ValueError) as excinfo:
        with added_connectors([("SmartLeads", SmartLeads)]):
            SmartLeads.manifest(test_connectors_directory)

    assert "No directory found for connector SmartLeadsX" in excinfo.value.args[0]
    assert "/src/hrflow_connectors/connectors/smartleadsx" in excinfo.value.args[0]


def test_manifest_logo_is_missing(test_connectors_directory):
    LocalUsers = SmartLeadsF()
    LocalUsers.model.name = "LocalUsers"
    LocalUsers.model.subtype = "localusers"
    with pytest.raises(ValueError) as excinfo:
        with added_connectors([("LocalUsers", LocalUsers)]):
            LocalUsers.manifest(test_connectors_directory)

    assert "Missing logo for connector LocalUsers" in excinfo.value.args[0]
    assert "/src/hrflow_connectors/connectors/localusers" in excinfo.value.args[0]


def test_manifest_more_than_one_logo(test_connectors_directory):
    SmartLeads = SmartLeadsF()
    with tempfile.NamedTemporaryFile(
        dir=test_connectors_directory / "smartleads",
        prefix="logo.",
    ):
        with pytest.raises(ValueError) as excinfo:
            with added_connectors([("SmartLeads", SmartLeads)]):
                SmartLeads.manifest(test_connectors_directory)

    assert "Found multiple logos for connector SmartLeads" in excinfo.value.args[0]


def test_manifest_logo_above_size_limit(test_connectors_directory):
    above_limit_size = 2 * MAX_LOGO_SIZE_BYTES
    with tempfile.NamedTemporaryFile(
        "wb",
        buffering=0,
        dir=test_connectors_directory / "localusers",
        prefix="logo.",
    ) as large_logo:
        large_logo.write(bytes([255] * above_limit_size))
        LocalUsers = SmartLeadsF()
        LocalUsers.model.name = "LocalUsers"
        LocalUsers.model.subtype = "localusers"
        with pytest.raises(ValueError) as excinfo:
            with added_connectors([("LocalUsers", LocalUsers)]):
                LocalUsers.manifest(test_connectors_directory)

    assert (
        f"Logo size {above_limit_size // 1024} KB for connector LocalUsers is"
        f" above maximum limit of {MAX_LOGO_SIZE_BYTES // 1024 } KB"
        in excinfo.value.args[0]
    )


def test_manifest_logo_not_valid_image(test_connectors_directory):
    with tempfile.NamedTemporaryFile(
        "wb",
        buffering=0,
        dir=test_connectors_directory / "localusers",
        prefix="logo.",
    ):
        LocalUsers = SmartLeadsF()
        LocalUsers.model.name = "LocalUsers"
        LocalUsers.model.subtype = "localusers"
        with pytest.raises(ValueError) as excinfo:
            with added_connectors([("LocalUsers", LocalUsers)]):
                LocalUsers.manifest(test_connectors_directory)

    assert "Logo file for connector LocalUsers" in excinfo.value.args[0]
    assert "doesn't seem to be a valid image" in excinfo.value.args[0]


MIDDLE_SIZE = (MIN_LOGO_PIXEL + MAX_LOGO_PIXEL) // 2


@pytest.mark.parametrize(
    "shape",
    [
        (MAX_LOGO_PIXEL + 1, MIDDLE_SIZE),
        (MIN_LOGO_PIXEL - 1, MIDDLE_SIZE),
        (MIDDLE_SIZE, MAX_LOGO_PIXEL + 1),
        (MIDDLE_SIZE, MIN_LOGO_PIXEL - 1),
        (MAX_LOGO_PIXEL + 1, MIN_LOGO_PIXEL - 1),
        (MIN_LOGO_PIXEL - 1, MAX_LOGO_PIXEL + 1),
        (MAX_LOGO_PIXEL + 1, MAX_LOGO_PIXEL + 1),
        (MIN_LOGO_PIXEL - 1, MIN_LOGO_PIXEL - 1),
        (MAX_LOGO_PIXEL - 1, MAX_LOGO_PIXEL - 2),
    ],
)
def test_manifest_logo_bad_dimension(test_connectors_directory, shape):
    original = Image.open(test_connectors_directory / "smartleads" / "logo.jpeg")
    with tempfile.NamedTemporaryFile(
        "wb",
        buffering=0,
        dir=test_connectors_directory / "localusers",
        prefix="logo.",
        suffix=".png",
    ) as bad_shape_logo:
        resized = original.resize(shape)
        resized.save(bad_shape_logo)

        LocalUsers = SmartLeadsF()
        LocalUsers.model.subtype = "localusers"
        with pytest.raises(ValueError) as excinfo:
            with added_connectors([("LocalUsers", LocalUsers)]):
                LocalUsers.manifest(test_connectors_directory)

    assert "Bad logo dimensions" in excinfo.value.args[0]


def test_manifest_includes_jsonmap_when_file_exists(test_connectors_directory):
    connector_directory = test_connectors_directory / SmartLeadsF().model.subtype
    format_mappings_directory = connector_directory / "mappings" / "format"
    connector = SmartLeadsF()

    format_mappings_directory.mkdir(parents=True, exist_ok=True)

    for action in connector.model.actions:
        jsonmap_file = format_mappings_directory / f"{action.name.value}.json"
        jsonmap_content = {"key": "value"}
        jsonmap_file.write_text(json.dumps(jsonmap_content))

    with added_connectors([("SmartLeads", connector)]):
        manifest = connector.manifest(connectors_directory=test_connectors_directory)

    for action_manifest in manifest["actions"]:
        assert "jsonmap" in action_manifest
        assert action_manifest["jsonmap"] == {"key": "value"}

    # Tear down
    shutil.rmtree(connector_directory / "mappings")


def test_manifest_includes_empty_jsonmap_when_file_missing(test_connectors_directory):
    connector_directory = test_connectors_directory / SmartLeadsF().model.subtype
    format_mappings_directory = connector_directory / "mappings" / "format"
    connector = SmartLeadsF()

    format_mappings_directory.mkdir(parents=True, exist_ok=True)

    with added_connectors([("SmartLeads", connector)]):
        manifest = connector.manifest(connectors_directory=test_connectors_directory)

    for action_manifest in manifest["actions"]:
        assert "jsonmap" in action_manifest
        assert action_manifest["jsonmap"] == {}

    # Tear down
    shutil.rmtree(connector_directory / "mappings")
