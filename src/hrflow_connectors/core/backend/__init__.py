import logging
import os
import typing as t

from hrflow_connectors.core.backend.common import BackendStore

ACTIVE_STORES: list[BackendStore] = []
from hrflow_connectors.core.backend.localjson import LocalJsonStore  # noqa

ACTIVE_STORES.append(LocalJsonStore)
try:
    from hrflow_connectors.core.backend.s3 import S3Store

    ACTIVE_STORES.append(S3Store)  # pragma: nocover
except ModuleNotFoundError:  # pragma: nocover
    pass  # pragma: nocover

logger = logging.getLogger(__name__)
store: t.Optional[BackendStore] = None

ENABLE_STORE_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_STORE_ENABLED"
STORE_NAME_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_STORE"
DEFAULT_STORE = LocalJsonStore.name


NAME_TO_STORE = {store.name: store for store in ACTIVE_STORES}


def configure_store():
    global store

    enable_store = os.environ.get(ENABLE_STORE_ENVIRONMENT_VARIABLE, None)
    if not enable_store or enable_store.lower() in ["false", "0"]:
        logger.info("No backend configured. Incremental mode is not possible.")
        store = None
        return

    store_name = os.environ.get(STORE_NAME_ENVIRONMENT_VARIABLE, DEFAULT_STORE)
    try:
        store = NAME_TO_STORE[store_name]
    except KeyError:
        raise Exception(
            "{}='{}' is not a valid store use one of {}".format(
                STORE_NAME_ENVIRONMENT_VARIABLE,
                store_name,
                list(NAME_TO_STORE.keys()),
            )
        )

    logger.info("Starting {} Backend configuration".format(store_name))
    store.init()
