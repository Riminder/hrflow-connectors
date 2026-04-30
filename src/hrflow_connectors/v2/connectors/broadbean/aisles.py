import hashlib
import hmac
import time
import typing as t
from logging import LoggerAdapter

import requests
from msgspec import Meta, Struct
from typing_extensions import Annotated

from hrflow_connectors.v2.connectors.broadbean.schemas import BroadbeanCandidate
from hrflow_connectors.v2.core.common import Entity
from hrflow_connectors.v2.core.warehouse import (
    Aisle,
    Criterias,
    Endpoint,
    Endpoints,
    WriteOperation,
    merge,
)

BROADBEAN_PUSH_ENDPOINT = (  # this endpoint is to be transmitted to bradbean
    "https://candidatehub.adcourier.com/candidate"
)
POST_CANDIDATE_ENDPOINT = Endpoint(
    name="Send candidate",
    description=(
        "This route accepts a POST request consisting of a JSON payload "
        "and some authentication Headers. The response will consist of a "
        "success flag and either a transaction ID (success) or an error."
    ),
    url=(
        "https://integrations.broadbean.com/hc/en-us/articles/"
        "115004599865-Sending-a-Candidate-in-"
    ),
)


class AuthParameters(Struct):
    secret_key: Annotated[
        str,
        Meta(
            description="Secret provided by Veritone Hire",
        ),
    ]


class WriteProfilesParameters(Struct):
    client_id: Annotated[
        str,
        Meta(
            description=(
                "ID for the candidate importer client mapper (Provided by Veritone"
                " Hire)"
            ),
        ),
    ]
    source_id: Annotated[
        str,
        Meta(
            description=(
                "Transaction/Publisher Source ID to link to the transformer (Provided"
                " by Veritone Hire)"
            ),
        ),
    ]
    job_id: Annotated[
        str,
        Meta(
            description="The ID of the Job the candidate has applied to",
        ),
    ]
    shortlist_id: Annotated[
        t.Optional[str],
        Meta(
            description="Shortlist ID for the candidate	",
        ),
    ] = None
    aplitrak_email_address: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "If you are a Job Board integrating with Candidate Hub, it is important"
                " that you include the unique tracking link/email in the supplementary"
                " context. This helps us identify the vacancy the candidate has applied"
                " for.	"
            ),
        ),
    ] = None


def write(
    adapter: LoggerAdapter,
    auth_parameters: AuthParameters,
    parameters: WriteProfilesParameters,
    items: t.Iterable[t.Dict],
) -> t.List[t.Dict]:
    failed_profiles = []
    for profile in items:
        profile_payload = dict(
            transaction=dict(
                client_id=parameters.client_id,
                source_id=parameters.source_id,
            ),
            candidate=profile,
            context=dict(
                id=parameters.job_id,
                shortlist_id=parameters.shortlist_id,
                aplitrak_email_address=parameters.aplitrak_email_address,
            ),
        )

        # Authentication
        epoch_time = int(time.time())
        signature_payload = f"{parameters.source_id}|{epoch_time}"
        signature = hmac.new(
            key=auth_parameters.secret_key.encode(),
            msg=signature_payload.encode(),
            digestmod=hashlib.sha256,
        ).hexdigest()

        response = requests.post(
            BROADBEAN_PUSH_ENDPOINT,
            headers={
                "X-CHUB-SIGNATURE-TIME": str(epoch_time),
                "X-CHUB-SOURCE": parameters.source_id,
                "X-CHUB-SIGNATURE": signature,
            },
            json=profile_payload,
        )
        if response.status_code // 100 != 2 or response.json().get("success") != 1:
            adapter.error(
                "Failed to push profile to Broadbean job_id={}"
                " status_code={} response={}".format(
                    parameters.job_id,
                    response.status_code,
                    response.text,
                )
            )
            failed_profiles.append(profile)
    return failed_profiles


ProfilesAisle = Aisle(
    name=Entity.profile,
    schema=BroadbeanCandidate,
    write=WriteOperation(
        criterias=Criterias(
            create=WriteProfilesParameters,
        ),
        function=merge(
            create=write,
        ),
        endpoints=Endpoints(create=POST_CANDIDATE_ENDPOINT),
    ),
)
