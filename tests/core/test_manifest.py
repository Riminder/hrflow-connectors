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
)
from tests.core.test_connector import SmartLeadsF


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
    SmartLeadsF().manifest(test_connectors_directory)


def test_hrflow_connectors_manifest(manifest_directory, test_connectors_directory):
    manifest = Path(__file__).parent / "manifest.json"
    assert manifest.exists() is False

    connectors = [SmartLeadsF(), SmartLeadsF()]
    hrflow_connectors_manifest(
        connectors=connectors,
        directory_path=manifest_directory,
        connectors_directory=test_connectors_directory,
    )

    assert manifest.exists() is True
    assert len(json.loads(manifest.read_text())["connectors"]) == len(connectors)


def test_manifest_connector_directory_not_found(test_connectors_directory):
    SmartLeads = SmartLeadsF()
    SmartLeads.model.name = "SmartLeadsX"
    with pytest.raises(ValueError) as excinfo:
        SmartLeads.manifest(test_connectors_directory)

    assert "No directory found for connector SmartLeadsX" in excinfo.value.args[0]
    assert "/src/hrflow_connectors/connectors/smartleadsx" in excinfo.value.args[0]


def test_manifest_logo_is_missing(test_connectors_directory):
    LocalUsers = SmartLeadsF()
    LocalUsers.model.name = "LocalUsers"
    with pytest.raises(ValueError) as excinfo:
        LocalUsers.manifest(test_connectors_directory)

    assert "Missing logo for connector LocalUsers" in excinfo.value.args[0]
    assert "/src/hrflow_connectors/connectors/localusers" in excinfo.value.args[0]


def test_manifest_more_than_one_logo(test_connectors_directory):
    with tempfile.NamedTemporaryFile(
        dir=test_connectors_directory / "smartleads",
        prefix="logo.",
    ):
        with pytest.raises(ValueError) as excinfo:
            SmartLeadsF().manifest(test_connectors_directory)

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
        with pytest.raises(ValueError) as excinfo:
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
        with pytest.raises(ValueError) as excinfo:
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
        LocalUsers.model.name = "LocalUsers"
        with pytest.raises(ValueError) as excinfo:
            LocalUsers.manifest(test_connectors_directory)

    assert "Bad logo dimensions" in excinfo.value.args[0]


def test_manifest_includes_jsonmap_when_file_exists(test_connectors_directory):
    connector_directory = test_connectors_directory / SmartLeadsF().model.name.lower()
    format_mappings_directory = connector_directory / "mappings" / "format"
    connector = SmartLeadsF()

    format_mappings_directory.mkdir(parents=True, exist_ok=True)

    for action in connector.model.actions:
        jsonmap_file = format_mappings_directory / f"{action.name.value}.json"
        jsonmap_content = {"key": "value"}
        jsonmap_file.write_text(json.dumps(jsonmap_content))

    manifest = connector.manifest(connectors_directory=test_connectors_directory)

    for action_manifest in manifest["actions"]:
        assert "jsonmap" in action_manifest
        assert action_manifest["jsonmap"] == {"key": "value"}

    # Tear down
    shutil.rmtree(connector_directory / "mappings")


def test_manifest_includes_empty_jsonmap_when_file_missing(test_connectors_directory):
    connector_directory = test_connectors_directory / SmartLeadsF().model.name.lower()
    format_mappings_directory = connector_directory / "mappings" / "format"
    connector = SmartLeadsF()

    format_mappings_directory.mkdir(parents=True, exist_ok=True)

    manifest = connector.manifest(connectors_directory=test_connectors_directory)

    for action_manifest in manifest["actions"]:
        assert "jsonmap" in action_manifest
        assert action_manifest["jsonmap"] == {}

    # Tear down
    shutil.rmtree(connector_directory / "mappings")
