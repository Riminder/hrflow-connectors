from contextvars import ContextVar

MAIN_IMPORT_NAME: ContextVar[str] = ContextVar(
    "MAIN_IMPORT_NAME", default="hrflow_connectors.v2"
)
