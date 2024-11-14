import re
import typing as t
from pathlib import Path

import pytest


def connectors() -> t.Optional[list[str]]:
    with open(
        Path(__file__).parent.parent.parent
        / "src"
        / "hrflow_connectors"
        / "__init__.py",
        "rt",
    ) as init_file:
        content = init_file.read()

    connectors = re.search(
        r"__CONNECTORS__\s*=\s*\[([^]]+),\s*\]", content, flags=re.MULTILINE
    )
    if connectors is None:
        return None
    return [connector.strip() for connector in connectors.group(1).strip().split(",")]


def test_v1_connectors_importable_from_root():
    v1_connectors = connectors()

    print(v1_connectors)
    assert v1_connectors is not None and len(v1_connectors) > 0

    script = (
        "\n".join(
            [
                f"from hrflow_connectors import {connector}"
                for connector in v1_connectors
            ]
        )
        + "\nno_error = True"
    )
    global_namespace = {}

    exec(script, global_namespace)

    assert global_namespace["no_error"] is True


# After moving current code into v1 folder some imports like
# from hrflow_connectors.connectors.xxxx.utils import yyy
# are expected to fail because the correct path should have **v1** like below
# from hrflow_connectors.**v1**.connectors.xxxx.utils import yyy
# But since this migration should not break code currently running in workflows
# all these legacy imports are reimported temporarly
# To find these import search an updated version of workflows-customers repository
LEGACY_IMPORTS = [
    "from hrflow_connectors.connectors.bullhorn.utils.authentication import auth"
]


@pytest.mark.parametrize("legacy_supported_import", LEGACY_IMPORTS)
def test_supported_legacy_imports_are_working(legacy_supported_import):
    script = f"{legacy_supported_import}\nno_error = True"
    global_namespace = {}

    exec(script, global_namespace)

    assert global_namespace["no_error"] is True
