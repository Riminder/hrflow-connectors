import json
import typing as t

from hrflow_connectors.connectors.hrflow.warehouse.profile import HrFlowProfileWarehouse
from hrflow_connectors.connectors.salesforce.schemas import SalesforceHrFlowProfile
from hrflow_connectors.connectors.salesforce.warehouse import SalesforceProfileWarehouse
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)


def format_profile(data: SalesforceHrFlowProfile) -> t.Dict:
    info = dict(
        full_name=f"{data['Last_Name__c']} {data['First_Name__c']}",
        first_name=data["First_Name__c"],
        last_name=data["Last_Name__c"],
        email=data["Email__c"],
        phone=data["Phone__c"],
        date_birth=data["Date_Birth__c"],
        location=dict(
            text=data["Location_Text__c"],
            lat=data["Location_Lat__c"],
            lng=data["Location_Lng__c"],
        ),
        gender=data["Gender__c"],
    )
    experiences = []
    if data["HrFlow_Profile_Experiences__r"]:
        experiences = [
            dict(
                title=experience["Title__c"],
                location=dict(
                    text=experience["Location_Text__c"],
                    lat=experience["Location_Lat__c"],
                    lng=experience["Location_Lng__c"],
                ),
                company=experience["Company__c"],
                date_start=dict(iso8601=experience["Date_Begin__c"]),
                date_end=dict(iso8601=experience["Date_End__c"]),
                description=experience["Description__c"],
                skills=json.loads(experience["Skills__c"]),
                tasks=json.loads(experience["Tasks__c"]),
                certifications=json.loads(experience["Certifications__c"]),
            )
            for experience in data["HrFlow_Profile_Experiences__r"]["records"]
        ]
    educations = []
    if data["HrFlow_Profile_Educations__r"]:
        educations = [
            dict(
                title=education["Title__c"],
                location=dict(
                    text=education["Location_Text__c"],
                    lat=education["Location_Lat__c"],
                    lng=education["Location_Lng__c"],
                ),
                school=education["School__c"],
                date_start=dict(iso8601=education["Date_Begin__c"]),
                date_end=dict(iso8601=education["Date_End__c"]),
                description=education["Description__c"],
                skills=json.loads(education["Skills__c"]),
                tasks=json.loads(education["Tasks__c"]),
                certifications=json.loads(education["Certifications__c"]),
                courses=json.loads(education["Courses__c"]),
            )
            for education in data["HrFlow_Profile_Educations__r"]["records"]
        ]
    attachments = []
    if data["HrFlow_Profile_Attachments__r"]:
        attachments = [
            dict(
                text=attachment["Text__c"],
                type=attachment["Type__c"],
                alt=attachment["Alt__c"],
                file_size=attachment["File_Size__c"],
                file_name=attachment["File_Name__c"],
                original_file_name=attachment["Original_File_Name__c"],
                extension=attachment["Extension__c"],
                url=attachment["URL__c"],
            )
            for attachment in data["HrFlow_Profile_Attachments__r"]["records"]
        ]
    return dict(
        key=data["Hash_Id__c"],
        reference=data["Reference__c"],
        archived_at=data["Archive__c"],
        updated_at=data["Date_Edition__c"],
        created_at=data["Timestamp__c"],
        info=info,
        text_language=data["Text_Language__c"],
        text=data["Text__c"],
        educations_duration=data["Experiences_Duration__c"],
        experiences=experiences,
        educations=educations,
        attachments=attachments,
        skills=json.loads(data["Skills__c"]),
        languages=json.loads(data["Languages__c"]),
        certifications=json.loads(data["Certifications__c"]),
        courses=json.loads(data["Courses__c"]),
        tasks=json.loads(data["Tasks__c"]),
        interests=json.loads(data["Interests__c"]),
        labels=json.loads(data["Labels__c"]),
        tags=json.loads(data["Tags__c"]),
        metadatas=json.loads(data["Metadatas__c"]),
    )


DESCRIPTION = (
    "Salesforce is the customer company. We make cloud-based software "
    "designed to help businesses connect to their customers in a whole "
    "new way, so they can find more prospects, close more deals, and wow"
    " customers with amazing service."
)

Salesforce = Connector(
    name="Salesforce",
    description=DESCRIPTION,
    url="https://www.salesforce.com",
    actions=[
        ConnectorAction(
            name=ActionName.pull_profile_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves profiles from Salesforce HrFlow Profile & Co Custom Objects"
                " and writes them to an Hrflow.ai source"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadProfileActionParameters", format=format_profile
            ),
            origin=SalesforceProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
