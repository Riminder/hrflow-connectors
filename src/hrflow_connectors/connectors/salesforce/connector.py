import json
import typing as t

from hrflow_connectors.connectors.hrflow.schemas import HrFlowProfile
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
    HrFlowProfileWarehouse,
)
from hrflow_connectors.connectors.salesforce.schemas import (
    SalesforceHrFlowJob,
    SalesforceHrFlowProfile,
)
from hrflow_connectors.connectors.salesforce.warehouse import (
    SalesforceJobWarehouse,
    SalesforceProfileWarehouse,
)
from hrflow_connectors.core import (
    ActionName,
    ActionType,
    BaseActionParameters,
    Connector,
    ConnectorAction,
    ConnectorType,
    WorkflowType,
)


def format_into_hrflow_profile(data: SalesforceHrFlowProfile) -> t.Dict:
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
        created_at=data["Date_Reception__c"],
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


def format_into_salesforce_profile(data: HrFlowProfile) -> t.Dict:
    experiences = None
    if data["experiences"]:
        experiences = dict(
            done=True,
            totalSize=len(data["experiences"]),
            records=[
                dict(
                    Certifications__c=json.dumps(experience["certifications"]),
                    Company__c=experience["company"],
                    Courses__c=json.dumps(experience["courses"]),
                    Date_Begin__c=experience["date_start"],
                    Date_End__c=experience["date_end"],
                    Description__c=experience["description"],
                    Hash_Id__c=experience["key"],
                    Location_Fields__c=json.dumps(experience["location"]["fields"]),
                    Location_Lat__c=experience["location"]["lat"],
                    Location_Lng__c=experience["location"]["lng"],
                    Location_Text__c=experience["location"]["text"],
                    Location_Gmaps__c=experience["location"]["gmaps"],
                    Skills__c=json.dumps(experience["skills"]),
                    Tasks__c=json.dumps(experience["tasks"]),
                    Title__c=experience["title"],
                )
                for experience in data["experiences"]
            ],
        )
    educations = None
    if data["educations"]:
        educations = dict(
            done=True,
            totalSize=len(data["educations"]),
            records=[
                dict(
                    Certifications__c=json.dumps(education["certifications"]),
                    School__c=education["school"],
                    Courses__c=json.dumps(education["courses"]),
                    Date_Begin__c=education["date_start"],
                    Date_End__c=education["date_end"],
                    Description__c=education["description"],
                    Hash_Id__c=education["key"],
                    Location_Fields__c=json.dumps(education["location"]["fields"]),
                    Location_Lat__c=education["location"]["lat"],
                    Location_Lng__c=education["location"]["lng"],
                    Location_Text__c=education["location"]["text"],
                    Location_Gmaps__c=education["location"]["gmaps"],
                    Skills__c=json.dumps(education["skills"]),
                    Tasks__c=json.dumps(education["tasks"]),
                    Title__c=education["title"],
                )
                for education in data["educations"]
            ],
        )
    attachments = None
    if data["attachments"]:
        attachments = dict(
            done=True,
            totalSize=len(data["attachments"]),
            records=[
                dict(
                    Alt__c=attachment["alt"],
                    Date_Edition__c=attachment["updated_at"],
                    Extension__c=attachment["extension"],
                    File_Name__c=attachment["file_name"],
                    File_Size__c=attachment["file_size"],
                    Original_File_Name__c=attachment["original_file_name"],
                    Timestamp__c=attachment["created_at"],
                    Type__c=attachment["type"],
                    URL__c=attachment["public_url"],
                )
                for attachment in data["attachments"]
            ],
        )
    return dict(
        Id__c=data["id"],
        Hash_Id__c=data["key"],
        Reference__c=data["reference"],
        Archive__c=data["archived_at"],
        Date_Edition__c=data["updated_at"],
        Date_Reception__c=data["created_at"],
        First_Name__c=data["info"]["first_name"],
        Last_Name__c=data["info"]["last_name"],
        Email__c=data["info"]["email"],
        Phone__c=data["info"]["phone"],
        Date_Birth__c=data["info"]["date_birth"],
        Location_Fields__c=json.dumps(data["info"]["location"]["fields"]),
        Location_Lat__c=data["info"]["location"]["lat"],
        Location_Lng__c=data["info"]["location"]["lng"],
        Location_Text__c=data["info"]["location"]["text"],
        Location_Gmaps__c=data["info"]["location"]["gmaps"],
        URLs__c=json.dumps(data["info"]["urls"]),
        Picture__c=data["info"]["picture"],
        Gender__c=data["info"]["gender"],
        Summary__c=data["info"]["summary"],
        Text_Language__c=data["text_language"],
        Text__c=data["text"],
        Experiences_Duration__c=data["experiences_duration"],
        Educations_Duration__c=data["educations_duration"],
        HrFlow_Profile_Experiences__r=experiences,
        HrFlow_Profile_Educations__r=educations,
        HrFlow_Profile_Attachments__r=attachments,
        Skills__c=json.dumps(data["skills"]),
        Languages__c=json.dumps(data["languages"]),
        Certifications__c=json.dumps(data["certifications"]),
        Courses__c=json.dumps(data["courses"]),
        Tasks__c=json.dumps(data["tasks"]),
        Interests__c=json.dumps(data["interests"]),
        Labels__c=json.dumps(data["labels"]),
        Tags__c=json.dumps(data["tags"]),
        Metadatas__c=json.dumps(data["metadatas"]),
    )


def format_job(data: SalesforceHrFlowJob) -> t.Dict:
    return dict(
        archived_at=data["Archive__c"],
        archive=data["Archive__c"],
        name=data["Name__c"],
        reference=data["Reference__c"],
        url=data["URL__c"],
        picture=data["Picture__c"],
        summary=data["Summary__c"],
        location=dict(
            text=data["Location_Text__c"],
            lat=data["Location_Lat__c"],
            lng=data["Location_Lng__c"],
        ),
        culture=data["Culture__c"],
        responsibilities=data["Responsibilities__c"],
        requirements=data["Requirements__c"],
        benefits=data["Benefits__c"],
        interviews=data["Interviews__c"],
        sections=json.loads(data["Sections__c"]),
        skills=json.loads(data["Skills__c"]),
        languages=json.loads(data["Languages__c"]),
        tags=json.loads(data["Tags__c"]),
        ranges_date=json.loads(data["Ranges_Date__c"]),
        ranges_float=json.loads(data["Ranges_Float__c"]),
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
    type=ConnectorType.CRM,
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
                "ReadProfileActionParameters", format=format_into_hrflow_profile
            ),
            origin=SalesforceProfileWarehouse,
            target=HrFlowProfileWarehouse,
            action_type=ActionType.inbound,
        ),
        ConnectorAction(
            name=ActionName.push_profile,
            trigger_type=WorkflowType.catch,
            description=(
                "Pushs specific Profile from HrFlow and writes"
                " it to HrFlow_Profile__c & Co Custom Objects in Salesforce"
            ),
            parameters=BaseActionParameters.with_defaults(
                "PushProfileActionParameters", format=format_into_salesforce_profile
            ),
            origin=HrFlowProfileWarehouse,
            target=SalesforceProfileWarehouse,
            action_type=ActionType.outbound,
        ),
        ConnectorAction(
            name=ActionName.pull_job_list,
            trigger_type=WorkflowType.pull,
            description=(
                "Retrieves jobs from Salesforce HrFlow Job Custom Object"
                " and writes them to an Hrflow.ai board"
            ),
            parameters=BaseActionParameters.with_defaults(
                "ReadJobActionParameters", format=format_job
            ),
            origin=SalesforceJobWarehouse,
            target=HrFlowJobWarehouse,
            action_type=ActionType.inbound,
        ),
    ],
)
