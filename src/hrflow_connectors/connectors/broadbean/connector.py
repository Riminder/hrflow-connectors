import base64
import magic
import datetime
import typing as t
from hrflow_connectors.connectors.broadbean.warehouse import BroadbeanCandidateWarehouse, BroadbeanProfileWarehouse

from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.hrflow.warehouse.profile import HrFlowProfileParsingWarehouse
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)



def get_profile_city(hrflow_location: t.Dict) -> str:
    fields = hrflow_location["fields"] or {}
    return fields.get("city") or "Undefined"

def get_profile_country(hrflow_location: t.Dict) -> str:
    fields = hrflow_location["fields"] or {}
    return fields.get("country") or "Undefined"

def get_profile_postcode(hrflow_location: t.Dict) -> str:
        fields = hrflow_location["fields"] or {}
        return fields.get("postcode") or "Undefined"

def get_profile_document(hrflow_profile: t.Dict) -> t.List:
    documents = []
    cv = next((attachment for attachment in hrflow_profile["attachments"] if attachment.get("type") == "original"))["public_url"]
    encoded_cv= base64.b64encode(cv).decode('utf-8')
    document1=dict(
            filename="CV",
            type="cv",
            content=encoded_cv
    )
    documents.append(document1)
    return documents


def format_profile_push(hrflow_profile: t.Dict) -> t.Dict:
    hrflow_profile_info = hrflow_profile["info"]
    profile = dict(
        first_name=hrflow_profile_info["first_name"],
        last_name=hrflow_profile_info["last_name"],
        email=hrflow_profile_info["email"],
        contact_telephone=hrflow_profile_info["phone"] or "Udefined",
        mobile_telephone=hrflow_profile_info["phone"] or "Undefiend",
        location_city=get_profile_city(hrflow_profile_info["location"]),
        location_country=get_profile_country(hrflow_profile_info["location"]),
        location_postcode=get_profile_postcode(hrflow_profile_info["location"]),
        location_latitude=hrflow_profile_info["location"]["lat"] or 0,
        location_longitude=hrflow_profile_info["location"]["lat"] or 0,
        current_job_title=None,
        current_job_employer=None,
        current_job_startdate=None,
        document=get_profile_document(hrflow_profile),
        context=dict(job_id=None, shortlist_id=None, aplitrak_email_address=None) #job_id field filled in write function 
    )
    return profile

def get_binary_resume(broadbean_profile: t.Dict) -> t.List:
    base64_filecontent =  next(element for element in broadbean_profile["documents"] if element["type"] == "cv")
    byte_array =  base64.b64decode(base64_filecontent)
    return bytearray

def get_content_type(broadbean_profile: t.Dict) -> str:
    encoded_data = next(element for element in broadbean_profile["documents"] if element["type"] == "cv")
    data =  base64.b64decode(encoded_data)
    content_type = magic.from_buffer(data)
    return content_type

def format_profile_catch(broadbean_profile: t.Dict) -> t.Dict:
    """
    Format the input data into a push-ready data schema
    Args:
        request (Dict[str, Any]): body we want to adapt to the output format
    Returns:
        Dict[str, Any]: parameters to put in the parsing endpoint
    """

    hrflow_tags = [{"name": "JobRefID", "value": broadbean_profile["context"]["job_id"] or None}]

    output_data = {
        "resume": {
            "raw": get_binary_resume(broadbean_profile),
            "content_type": get_content_type(broadbean_profile),
        },
        "reference": broadbean_profile["ResumeValue"],
        "tags": hrflow_tags,
        "metadatas": [],
        "created_at": datetime.datetime.now().isoformat(),
    }
    return output_data



DESCRIPTION = (
    "Broadbean is a software platform for recruitment and talent acquisition. "
    "It helps companies and recruiters find, attract, and hire the best candidates "
    "for open positions. It offers features such as job posting, resume search, "
    "candidate tracking, and reporting and analytics. Broadbean is used by "
    "companies across a wide range of industries and is a popular choice for " 
    "recruiters and talent acquisition professionals who want to streamline their "
    "recruitment process."
)
SmartRecruiters = Connector(
    name="Broadbean",
    description=DESCRIPTION,
    url="https://www.broadbean.com",
    actions=[
        ConnectorAction(
            name="catch_candidates",
            trigger_type=WorkflowType.catch,
            description=(
                "Retrieves a candidate via the ***Broadbean*** API and send them"
                " to ***Hrflow.ai Parsing API***."
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadCandidateActionParameters", format=format_profile_catch
            ),
            origin=BroadbeanCandidateWarehouse,
            target=HrFlowProfileParsingWarehouse,
        ),
        ConnectorAction(
            name="push_profile",
            trigger_type=WorkflowType.catch,
            description=(
                "Writes a profile from Hrflow.ai Source to Broadbean via the API"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile_push
            ),
            origin=HrFlowProfileWarehouse,
            target=BroadbeanProfileWarehouse,
        ),
    ],
)
