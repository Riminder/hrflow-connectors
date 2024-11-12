from unittest import mock

import pytest

from hrflow_connectors.v2.core.utils import (
    AmbiguousConnectorImportName,
    ConnectorImportNameNotFound,
    NoDefFunctionNameFound,
    get_import_name,
    reindent_function_source,
)


def test_get_import_name_fails_if_connector_not_found():
    with pytest.raises(ConnectorImportNameNotFound):
        with mock.patch(
            "hrflow_connectors.v2.core.utils.inspect.getmembers", return_value=[]
        ):
            get_import_name(mock.MagicMock())


def test_get_import_name_fails_if_more_than_one_symbol_found():
    with pytest.raises(AmbiguousConnectorImportName):
        with mock.patch(
            "hrflow_connectors.v2.core.utils.inspect.getmembers",
            return_value=[(1, 1), (1, 1)],
        ):
            get_import_name(mock.MagicMock())


@pytest.mark.parametrize(
    "source",
    [
        """
def my_func(*args, **kwargs):
    for i in range(10):
        print(i)
        if i % 2 == 0:
            print("Even")
    return 
""",
        """
    def my_func(*args, **kwargs):
        for i in range(10):
            print(i)
            if i % 2 == 0:
                print("Even")
        return 
""",
        """
        def my_func(*args, **kwargs):
            for i in range(10):
                print(i)
                if i % 2 == 0:
                    print("Even")
            return 
""",
        """
\t\tdef my_func(*args, **kwargs):
\t\t    for i in range(10):
\t\t        print(i)
\t\t        if i % 2 == 0:
\t\t            print("Even")
\t\t    return 
""",
    ],
)
def test_reindent_function_source_works_as_expected(source: str):
    assert (
        reindent_function_source(source, "my_func")
        == """
def my_func(*args, **kwargs):
    for i in range(10):
        print(i)
        if i % 2 == 0:
            print("Even")
    return """
    )


def test_reindent_function_source_fails_if_source_has_no_def():
    with pytest.raises(NoDefFunctionNameFound):
        reindent_function_source(
            """
my_func = lambda a, b, c: dict()
""",
            "my_func",
        )


def test_reindent_function_source_fails_if_function_name_is_wrong():
    with pytest.raises(NoDefFunctionNameFound):
        reindent_function_source(
            """
def my_func():
    return None
""",
            "my_func_wrong",
        )
