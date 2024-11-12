import random
import string
import typing as t
from contextlib import ExitStack, contextmanager
from unittest import mock

from hrflow_connectors.v2.core.connector import Connector
from hrflow_connectors.v2.core.context import MAIN_IMPORT_NAME


def random_workflow_id() -> str:
    return "".join([random.choice(string.ascii_letters) for _ in range(14)])


@contextmanager
def added_connectors(
    symbols: t.Iterable[t.Tuple[str, Connector]],
    module: str = "hrflow_connectors.v2",
    *,
    create_module=False,
):
    with ExitStack() as stack:
        if create_module:
            if "." in module:
                raise Exception("create_module not supported for nested module")

            stack.enter_context(
                mock.patch.dict("sys.modules", **{module: mock.MagicMock()})
            )
        for name, connector in symbols:
            stack.enter_context(mock.patch(f"{module}.{name}", connector, create=True))
        yield


@contextmanager
def main_import_name_as(name: str):
    reset_token = MAIN_IMPORT_NAME.set(name)
    try:
        yield
    finally:
        MAIN_IMPORT_NAME.reset(reset_token)
