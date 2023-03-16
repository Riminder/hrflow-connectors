import logging
import os

ACTIVE_STORES = []
from hrflow_connectors.core.backend.localjson import LocalJsonStore  # noqa

ACTIVE_STORES.append(LocalJsonStore)
try:
    from hrflow_connectors.core.backend.s3 import S3Store

    ACTIVE_STORES.append(S3Store)  # pragma: nocover
except ModuleNotFoundError:  # pragma: nocover
    pass  # pragma: nocover

logger = logging.getLogger(__name__)
store = None
is_configured = False

ENABLE_STORE_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_STORE_ENABLED"
STORE_NAME_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_STORE"
DEFAULT_STORE = LocalJsonStore.NAME()


NAME_TO_STORE = {store.NAME(): store for store in ACTIVE_STORES}


def configure_store():
    global store, is_configured

    enable_store = os.environ.get(ENABLE_STORE_ENVIRONMENT_VARIABLE, None)
    if not enable_store or enable_store in ["false", "False", "0"]:
        logger.info("No backend configured. Incremental mode is not possible.")
        store = None
        is_configured = False
        return

    store_name = os.environ.get(STORE_NAME_ENVIRONMENT_VARIABLE, DEFAULT_STORE)
    try:
        store_class = NAME_TO_STORE[store_name]
    except KeyError:
        raise Exception(
            "{}='{}' is not a valid store use one of {}".format(
                STORE_NAME_ENVIRONMENT_VARIABLE,
                store_name,
                list(NAME_TO_STORE.keys()),
            )
        )

    logger.info("Starting {} Backend configuration".format(store_name))
    store = store_class()
    is_configured = True
