import logging
import os
import typing as t
from io import BytesIO

import boto3
import botocore
from pydantic import BaseModel, create_model

from hrflow_connectors.core.backend.common import BackendStore

logger = logging.getLogger(__name__)


class S3Store(BackendStore):
    BUCKET_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_S3_BUCKET"
    PREFIX_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_S3_PREFIX"
    AWS_REGION_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_S3_AWS_REGION"
    AWS_ACCESS_KEY_ID_ENVIRONMENT_VARIABLE = "HRFLOW_CONNECTORS_S3_AWS_ACCESS_KEY_ID"
    AWS_SECRET_ACCESS_KEY_ENVIRONMENT_VARIABLE = (
        "HRFLOW_CONNECTORS_S3_AWS_SECRET_ACCESS_KEY"
    )
    S3_FOLDER_NAME = "hrflow_connectors/store"

    def __init__(
        self,
    ) -> None:
        for environment_variable in [
            S3Store.BUCKET_ENVIRONMENT_VARIABLE,
            S3Store.AWS_REGION_ENVIRONMENT_VARIABLE,
        ]:
            if os.environ.get(environment_variable) is None:
                raise Exception(
                    "Missing environment variable {} in order to setup S3 store".format(
                        environment_variable
                    )
                )
        if os.environ.get(
            S3Store.AWS_ACCESS_KEY_ID_ENVIRONMENT_VARIABLE
        ) and os.environ.get(S3Store.AWS_SECRET_ACCESS_KEY_ENVIRONMENT_VARIABLE):
            logger.info("Configuring S3 Backend with explicit credentials")
            self.client = boto3.client(
                "s3",
                region_name=os.environ.get(S3Store.AWS_REGION_ENVIRONMENT_VARIABLE),
                aws_access_key_id=os.environ.get(
                    S3Store.AWS_ACCESS_KEY_ID_ENVIRONMENT_VARIABLE
                ),
                aws_secret_access_key=os.environ.get(
                    S3Store.AWS_SECRET_ACCESS_KEY_ENVIRONMENT_VARIABLE
                ),
            )
        else:
            logger.info("Configuring S3 Backend with implicit credentials")
            self.client = boto3.client(
                "s3",
                region_name=os.environ.get(S3Store.AWS_REGION_ENVIRONMENT_VARIABLE),
            )

        self.bucket = os.environ.get(S3Store.BUCKET_ENVIRONMENT_VARIABLE)
        self.key_prefix = S3Store.S3_FOLDER_NAME
        prefix = os.environ.get(S3Store.PREFIX_ENVIRONMENT_VARIABLE)
        if prefix is not None:
            self.key_prefix = "{}/{}".format(
                prefix.strip("/"), S3Store.S3_FOLDER_NAME.strip("/")
            )

        self.__check_store()

    @staticmethod
    def NAME() -> str:
        return "s3"

    def __check_store(self):
        root_model = create_model(
            "S3StoreRoot", root="HrFlow Connectors", store=S3Store.NAME()
        )
        root_key = "__root"
        root = root_model()
        try:
            self.save(root_key, root)
        except Exception as e:
            raise Exception("Failed to check writing to S3 with error={}".format(e))

        try:
            loaded = self.load(key=root_key, parse_as=root_model)
            assert loaded == root, "Loaded data should match original data"
        except Exception as e:
            raise Exception("Failed to check reading from S3 with error={}".format(e))
        logger.info("S3 Backend properly configured")

    def s3_key(self, key: str) -> str:
        return "{}/{}.json".format(self.key_prefix.strip("/"), key)

    def save(self, key: str, data: BaseModel) -> None:
        with BytesIO(initial_bytes=data.json().encode()) as raw:
            self.client.upload_fileobj(raw, self.bucket, self.s3_key(key))
        return None

    def load(self, key: str, parse_as: t.Type[BaseModel]) -> t.Optional[BaseModel]:
        with BytesIO() as raw:
            try:
                self.client.download_fileobj(self.bucket, self.s3_key(key), raw)
            except botocore.exceptions.ClientError as e:
                if int(e.response["Error"]["Code"]) == 404:
                    return None
                raise e
            return parse_as.parse_raw(raw.getvalue().decode())
