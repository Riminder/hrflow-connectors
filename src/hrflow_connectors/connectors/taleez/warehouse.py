import typing as t
from logging import LoggerAdapter
import re

import requests
from pydantic import BaseModel, Field

from hrflow_connectors.core import Warehouse, WarehouseWriteAction
from hrflow_connectors.connectors.taleez.schemas import Candidate


POST_CANDIDATE_ENDPOINT = "https://api.taleez.com/0/candidates"
ACCEPT = "application/json;charset=UTF-8"
CONTENT_TYPE = "application/json"


class WriteProfilesParameters(BaseModel):
    x_taleez_api_secret: str = Field(
        ..., description="Client Secret id used to access Taleez API", repr=Field
    )
    accept: str = Field(ACCEPT, const=True)
    content_type: str = Field(CONTENT_TYPE, const=True)


def write(
    adapter: LoggerAdapter,
    parameters: WriteProfilesParameters,
    data: t.Iterable[t.Dict],
) -> t.Iterable[t.Dict]:
    data = list(data)
    adapter.info("Pushing {} candidates to Taleez API".format(len(data)))
    failed_profiles = []
    for profile in data:
        response = requests.post(
            POST_CANDIDATE_ENDPOINT, json=profile["candidate"], headers=parameters
        )
        id = re.search("[0-9]{4,5}", response.content).group(0)
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create ad status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        # TODO: request to add documents and properties to candidate
        binary_cv = requests.get(profile["CV"]).content
        response = requests.post(
            "{}/{}/documents?cv=true".format(POST_CANDIDATE_ENDPOINT, id),
            files=[("file", (binary_cv, "application/pdf"))],
            headers=parameters,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create ad status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        # TODO: POST the properties to candidate after having created it
        response = requests.post(
            "{}/{}/properties".format(POST_CANDIDATE_ENDPOINT, id),
            data=profile["properties"],
            headers=parameters,
        )
        if response.status_code // 100 != 2:
            adapter.error(
                "Failed to create ad status_code={} response={}".format(
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
        # properties = get_profile_properties_to_push()

    return failed_profiles


TaleezProfilesWarehouse = Warehouse(
    name="Taleez Profiles Warehouse",
    data_schema=Candidate,
    write=WarehouseWriteAction(
        parameters=WriteProfilesParameters,
        function=write,
    ),
)
