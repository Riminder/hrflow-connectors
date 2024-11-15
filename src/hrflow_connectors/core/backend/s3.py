import logging
import os
import typing as t
from io import BytesIO

import boto3
import boto3.session
from msgspec import Struct, convert, defstruct, json
from pydantic import BaseModel, create_model

from hrflow_connectors.core.backend.common import BackendStore, msgspec_dec_hook

logger = logging.getLogger(__name__)

BUCKET_ENVIRONMENT_VARIABLE: t.Final = "HRFLOW_CONNECTORS_S3_BUCKET"
PREFIX_ENVIRONMENT_VARIABLE: t.Final = "HRFLOW_CONNECTORS_S3_PREFIX"
AWS_REGION_ENVIRONMENT_VARIABLE: t.Final = "HRFLOW_CONNECTORS_S3_AWS_REGION"
AWS_ACCESS_KEY_ID_ENVIRONMENT_VARIABLE: t.Final = (
    "HRFLOW_CONNECTORS_S3_AWS_ACCESS_KEY_ID"
)
AWS_SECRET_ACCESS_KEY_ENVIRONMENT_VARIABLE: t.Final = (
    "HRFLOW_CONNECTORS_S3_AWS_SECRET_ACCESS_KEY"
)
S3_FOLDER_NAME: t.Final = "hrflow_connectors/store"
NAME: t.Final = "s3"


class InternalStateBase(Struct):
    bucket: str
    key_prefix: str

    def s3_key(self, key: str) -> str:
        return "{}/{}.json".format(self.key_prefix.strip("/"), key)


if t.TYPE_CHECKING:
    from mypy_boto3_s3 import S3Client  # pragma: nocover

    class InternalState(InternalStateBase):  # pragma: nocover
        client: S3Client

else:

    class InternalState(InternalStateBase):
        client: t.Any


class CannotWriteToS3Error(Exception):
    pass


class CannotReadFromS3Error(Exception):
    pass


def check_store_pydantic(state: InternalState):
    root_model = create_model("S3StoreRoot", root="HrFlow Connectors", store=NAME)
    root_key = "__root"
    root = root_model()
    try:
        save(state, root_key, root)
    except Exception as e:
        raise CannotWriteToS3Error(
            "Failed to check writing to S3 with error={}".format(e)
        )

    try:
        loaded = load(state, key=root_key, parse_as=root_model)
        assert loaded == root, "Loaded data should match original data pydantic"
    except Exception as e:
        raise CannotReadFromS3Error(
            "Failed to check reading from S3 with error={}".format(e)
        )


def check_store_msgspec(state: InternalState):
    root_model = defstruct(
        "S3StoreRoot", [("root", str, "HrFlow Connectors"), ("store", str, NAME)]
    )
    root_key = "__root"
    root = root_model()
    try:
        save(state, root_key, root)
    except Exception as e:
        raise CannotWriteToS3Error(
            "Failed to check writing to S3 with error={}".format(e)
        )

    try:
        loaded = load(state, key=root_key, parse_as=root_model)
        assert loaded == root, "Loaded data should match original data msgspec"
    except Exception as e:
        raise CannotReadFromS3Error(
            "Failed to check reading from S3 with error={}".format(e)
        )


def check_store(state: InternalState):
    check_store_pydantic(state)
    check_store_msgspec(state)
    logger.info("S3 Backend properly configured")


def get_state():
    if (region_name := os.environ.get(AWS_REGION_ENVIRONMENT_VARIABLE)) is None:
        raise Exception(
            f"Missing environment variable {AWS_REGION_ENVIRONMENT_VARIABLE} in order"
            " to setup S3 store"
        )

    if (bucket := os.environ.get(BUCKET_ENVIRONMENT_VARIABLE)) is None:
        raise Exception(
            f"Missing environment variable {BUCKET_ENVIRONMENT_VARIABLE} for S3 store"
            " setup"
        )

    if os.environ.get(AWS_ACCESS_KEY_ID_ENVIRONMENT_VARIABLE) and os.environ.get(
        AWS_SECRET_ACCESS_KEY_ENVIRONMENT_VARIABLE
    ):
        logger.info("Configuring S3 Backend with explicit credentials")
        client = boto3.client(
            "s3",
            region_name=region_name,
            aws_access_key_id=os.environ.get(AWS_ACCESS_KEY_ID_ENVIRONMENT_VARIABLE),
            aws_secret_access_key=os.environ.get(
                AWS_SECRET_ACCESS_KEY_ENVIRONMENT_VARIABLE
            ),
        )
    else:
        logger.info("Configuring S3 Backend with implicit credentials")
        client = boto3.client(
            "s3",
            region_name=region_name,
        )

    key_prefix = S3_FOLDER_NAME
    prefix = os.environ.get(PREFIX_ENVIRONMENT_VARIABLE)
    if prefix is not None:
        key_prefix = "{}/{}".format(prefix.strip("/"), S3_FOLDER_NAME.strip("/"))

    state = InternalState(client=client, bucket=bucket, key_prefix=key_prefix)

    check_store(state)

    return state


def save(state: InternalState, key: str, data: t.Union[BaseModel, Struct]) -> None:
    if isinstance(data, BaseModel):
        with BytesIO(initial_bytes=data.json().encode()) as raw:
            state.client.upload_fileobj(raw, state.bucket, state.s3_key(key))
    else:
        with BytesIO(initial_bytes=json.encode(data)) as raw:
            state.client.upload_fileobj(raw, state.bucket, state.s3_key(key))


def load(
    state: InternalState, key: str, parse_as: t.Union[type[BaseModel], type[Struct]]
):
    with BytesIO() as raw:
        try:
            state.client.download_fileobj(state.bucket, state.s3_key(key), raw)
        except state.client.exceptions.ClientError as e:
            if int(e.response["Error"]["Code"]) == 404:
                return None
            raise e
        if issubclass(parse_as, BaseModel):
            return parse_as.parse_raw(raw.getvalue().decode())
        return convert(json.decode(raw.getvalue()), parse_as, dec_hook=msgspec_dec_hook)


S3Store = BackendStore(name=NAME, get_state=get_state, saver=save, loader=load)
