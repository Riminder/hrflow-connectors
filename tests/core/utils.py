import typing as t
from contextlib import ExitStack, contextmanager
from unittest import mock

from hrflow_connectors.core import Connector


@contextmanager
def added_connectors(*symbols: t.Tuple[str, Connector]):
    with ExitStack() as stack:
        for name, connector in symbols:
            stack.enter_context(
                mock.patch(f"hrflow_connectors.{name}", connector, create=True)
            )
        yield
