import io
import json
import shutil
import tempfile
from pathlib import Path

import pytest
from PIL import Image

from hrflow_connectors.v2.core.connector import hrflow_connectors_manifest
from hrflow_connectors.v2.core.utils import (
    MAX_LOGO_PIXEL,
    MAX_LOGO_SIZE_BYTES,
    MIN_LOGO_PIXEL,
    AmbiguousConnectorImportName,
    ConnectorImportNameNotFound,
)
from tests.v2.core.conftest import TypedSmartLeads
from tests.v2.core.utils import added_connectors, main_import_name_as


def test_connector_manifest(connectors_directory: Path, SmartLeads: TypedSmartLeads):
    with added_connectors([("SmartLeads", SmartLeads)]):
        manifest = SmartLeads.manifest(connectors_directory)

    assert len(manifest["actions"]) == len(SmartLeads.flows)


def test_connector_manifest_works_with_parameterized_main_module_name(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    parameterized_name = "third_party"

    with main_import_name_as(parameterized_name):
        # Should fail because by default SmartLeads is added to hrflow_connectors.v2
        with added_connectors([("SmartLeads", SmartLeads)]):
            with pytest.raises(ModuleNotFoundError):
                SmartLeads.manifest(connectors_directory)

        with added_connectors(
            [("SmartLeads", SmartLeads)], parameterized_name, create_module=True
        ):
            manifest = SmartLeads.manifest(connectors_directory)

    for action in manifest["actions"]:
        assert (
            f"from {parameterized_name} import SmartLeads"
            in action["workflow"]["catch_template"]
        )
        assert (
            f"from {parameterized_name} import SmartLeads"
            in action["workflow"]["pull_template"]
        )


def test_connector_manifest_fails_if_cannot_find_import_name(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    with pytest.raises(ConnectorImportNameNotFound):
        SmartLeads.manifest(connectors_directory)


def test_connector_manifest_fails_if_connector_misconfigured(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    with pytest.raises(AmbiguousConnectorImportName):
        with added_connectors([("SmartLeads", SmartLeads), ("Duplicated", SmartLeads)]):
            SmartLeads.manifest(connectors_directory)


def test_manifest_connector_directory_not_found(
    connectors_directory: Path, SmartLeads: TypedSmartLeads
):
    SmartLeads.name = "SmartLeadsX"
    SmartLeads.subtype = "smartleadsx"
    with pytest.raises(ValueError) as excinfo:
        with added_connectors([("SmartLeads", SmartLeads)]):
            SmartLeads.manifest(connectors_directory)

    assert "No directory found for connector SmartLeadsX" in excinfo.value.args[0]
    assert "/src/hrflow_connectors/connectors/smartleadsx" in excinfo.value.args[0]


@pytest.fixture
def remove_smartleads_logo(connectors_directory: Path, SmartLeads: TypedSmartLeads):
    logo = connectors_directory / SmartLeads.subtype / "logo.jpeg"
    logo_bytes = logo.read_bytes()

    logo.unlink()
    yield logo_bytes
    logo.write_bytes(logo_bytes)


def test_manifest_logo_is_missing(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
    remove_smartleads_logo: bytes,
):
    with pytest.raises(ValueError) as excinfo:
        with added_connectors([("SmartLeads", SmartLeads)]):
            SmartLeads.manifest(connectors_directory)

    assert "Missing logo for connector SmartLeads" in excinfo.value.args[0]
    assert "/src/hrflow_connectors/connectors/smartleads" in excinfo.value.args[0]


def test_manifest_more_than_one_logo(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    with tempfile.NamedTemporaryFile(
        dir=connectors_directory / SmartLeads.subtype,
        prefix="logo.",
    ):
        with pytest.raises(ValueError) as excinfo:
            with added_connectors([("SmartLeads", SmartLeads)]):
                SmartLeads.manifest(connectors_directory)

    assert "Found multiple logos for connector SmartLeads" in excinfo.value.args[0]


def test_manifest_logo_above_size_limit(
    SmartLeads: TypedSmartLeads,
    connectors_directory: Path,
    remove_smartleads_logo: bytes,
):
    above_limit_size = 2 * MAX_LOGO_SIZE_BYTES
    with tempfile.NamedTemporaryFile(
        "wb",
        buffering=0,
        dir=connectors_directory / SmartLeads.subtype,
        prefix="logo.",
    ) as large_logo:
        large_logo.write(bytes([255] * above_limit_size))
        with pytest.raises(ValueError) as excinfo:
            with added_connectors([("SmartLeads", SmartLeads)]):
                SmartLeads.manifest(connectors_directory)

    assert (
        f"Logo size {above_limit_size // 1024} KB for connector SmartLeads is"
        f" above maximum limit of {MAX_LOGO_SIZE_BYTES // 1024 } KB"
        in excinfo.value.args[0]
    )


def test_manifest_logo_not_valid_image(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
    remove_smartleads_logo: bytes,
):
    with tempfile.NamedTemporaryFile(
        "wb",
        buffering=0,
        dir=connectors_directory / SmartLeads.subtype,
        prefix="logo.",
    ):
        with pytest.raises(ValueError) as excinfo:
            with added_connectors([("SmartLeads", SmartLeads)]):
                SmartLeads.manifest(connectors_directory)

    assert "Logo file for connector SmartLeads" in excinfo.value.args[0]
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
def test_manifest_logo_bad_dimension(
    SmartLeads: TypedSmartLeads,
    connectors_directory: Path,
    shape: tuple[int, int],
    remove_smartleads_logo: bytes,
):
    original = Image.open(io.BytesIO(remove_smartleads_logo))
    with tempfile.NamedTemporaryFile(
        "wb",
        buffering=0,
        dir=connectors_directory / SmartLeads.subtype,
        prefix="logo.",
        suffix=".png",
    ) as bad_shape_logo:
        resized = original.resize(shape)
        resized.save(bad_shape_logo)

        with pytest.raises(ValueError) as excinfo:
            with added_connectors([("SmartLeads", SmartLeads)]):
                SmartLeads.manifest(connectors_directory)

    assert "Bad logo dimensions" in excinfo.value.args[0]


def test_manifest_includes_jsonmap_when_file_exists(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    connector_directory = connectors_directory / SmartLeads.subtype
    format_mappings_directory = connector_directory / "mappings" / "format"

    format_mappings_directory.mkdir(parents=True, exist_ok=True)

    for flow in SmartLeads.flows:
        jsonmap_file = (
            format_mappings_directory / f"{flow.name(SmartLeads.subtype)}.json"
        )
        jsonmap_content = {"key": "value"}
        jsonmap_file.write_text(json.dumps(jsonmap_content))

    with added_connectors([("SmartLeads", SmartLeads)]):
        manifest = SmartLeads.manifest(connectors_directory=connectors_directory)

    for action_manifest in manifest["actions"]:
        assert "jsonmap" in action_manifest
        assert action_manifest["jsonmap"] == {"key": "value"}

    # Tear down
    shutil.rmtree(connector_directory / "mappings")


def test_manifest_includes_empty_jsonmap_when_file_missing(
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    connector_directory = connectors_directory / SmartLeads.subtype
    format_mappings_directory = connector_directory / "mappings" / "format"

    format_mappings_directory.mkdir(parents=True, exist_ok=True)

    with added_connectors([("SmartLeads", SmartLeads)]):
        manifest = SmartLeads.manifest(connectors_directory=connectors_directory)

    for action_manifest in manifest["actions"]:
        assert "jsonmap" in action_manifest
        assert action_manifest["jsonmap"] == {}

    # Tear down
    shutil.rmtree(connector_directory / "mappings")


@pytest.fixture
def manifest_directory():
    path = Path(__file__).parent
    yield path
    manifest = path / "manifest.json"
    try:
        manifest.unlink()
    except FileNotFoundError:
        pass


def test_hrflow_connectors_manifest(
    manifest_directory: Path,
    connectors_directory: Path,
    SmartLeads: TypedSmartLeads,
):
    manifest = Path(__file__).parent / "manifest.json"
    assert manifest.exists() is False

    with added_connectors(
        [("SmartLeads", SmartLeads)],
    ):
        hrflow_connectors_manifest(
            connectors=[SmartLeads],
            directory_path=str(manifest_directory),
            connectors_directory=connectors_directory,
        )

    assert manifest.exists() is True
    assert len(json.loads(manifest.read_text())["connectors"]) == 1
