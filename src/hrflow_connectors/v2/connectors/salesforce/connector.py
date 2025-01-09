import json
import typing as t

from hrflow_connectors.v2.connectors.salesforce.warehouse import SalesforceWarehouse
from hrflow_connectors.v2.core.common import Direction, Entity, Mode
from hrflow_connectors.v2.core.connector import Connector, ConnectorType, Flow


def format_into_hrflow_profile(data: t.Dict) -> t.Dict:
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


def format_into_archive_in_hrlow(data: t.Dict) -> t.Dict:
    return dict(
        reference=data["Reference__c"],
    )


def format_into_salesforce_profile(hrflow_profile: t.Dict) -> t.Dict:
    experiences = None
    if hrflow_profile["experiences"]:
        experiences = dict(
            done=True,
            totalSize=len(hrflow_profile["experiences"]),
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
                for experience in hrflow_profile["experiences"]
            ],
        )
    educations = None
    if hrflow_profile["educations"]:
        educations = dict(
            done=True,
            totalSize=len(hrflow_profile["educations"]),
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
                for education in hrflow_profile["educations"]
            ],
        )
    attachments = None
    if hrflow_profile["attachments"]:
        attachments = dict(
            done=True,
            totalSize=len(hrflow_profile["attachments"]),
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
                for attachment in hrflow_profile["attachments"]
            ],
        )
    return dict(
        Id__c=hrflow_profile["id"],
        Hash_Id__c=hrflow_profile["key"],
        Reference__c=hrflow_profile["reference"],
        Archive__c=hrflow_profile["archived_at"],
        Date_Edition__c=hrflow_profile["updated_at"],
        Date_Reception__c=hrflow_profile["created_at"],
        First_Name__c=hrflow_profile["info"]["first_name"],
        Last_Name__c=hrflow_profile["info"]["last_name"],
        Email__c=hrflow_profile["info"]["email"],
        Phone__c=hrflow_profile["info"]["phone"],
        Date_Birth__c=hrflow_profile["info"]["date_birth"],
        Location_Fields__c=json.dumps(hrflow_profile["info"]["location"]["fields"]),
        Location_Lat__c=hrflow_profile["info"]["location"]["lat"],
        Location_Lng__c=hrflow_profile["info"]["location"]["lng"],
        Location_Text__c=hrflow_profile["info"]["location"]["text"],
        Location_Gmaps__c=hrflow_profile["info"]["location"]["gmaps"],
        URLs__c=json.dumps(hrflow_profile["info"]["urls"]),
        Picture__c=hrflow_profile["info"]["picture"],
        Gender__c=hrflow_profile["info"]["gender"],
        Summary__c=hrflow_profile["info"]["summary"],
        Text_Language__c=hrflow_profile["text_language"],
        Text__c=hrflow_profile["text"],
        Experiences_Duration__c=hrflow_profile["experiences_duration"],
        Educations_Duration__c=hrflow_profile["educations_duration"],
        HrFlow_Profile_Experiences__r=experiences,
        HrFlow_Profile_Educations__r=educations,
        HrFlow_Profile_Attachments__r=attachments,
        Skills__c=json.dumps(hrflow_profile["skills"]),
        Languages__c=json.dumps(hrflow_profile["languages"]),
        Certifications__c=json.dumps(hrflow_profile["certifications"]),
        Courses__c=json.dumps(hrflow_profile["courses"]),
        Tasks__c=json.dumps(hrflow_profile["tasks"]),
        Interests__c=json.dumps(hrflow_profile["interests"]),
        Labels__c=json.dumps(hrflow_profile["labels"]),
        Tags__c=json.dumps(hrflow_profile["tags"]),
        Metadatas__c=json.dumps(hrflow_profile["metadatas"]),
    )


def format_into_archive_in_salesforce(data: t.Dict) -> t.Dict:
    return dict(
        Reference__c=data["reference"],
    )


def format_job(data: t.Dict) -> t.Dict:
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
    "Salesforce, Inc. is an American cloud-based software company headquartered in San"
    " Francisco, California. It provides customer relationship management (CRM)"
    " software and applications focused on sales, customer service, marketing"
    " automation, e-commerce, analytics, and application development."
)

Salesforce = Connector(
    name="Salesforce",
    type=ConnectorType.CRM,
    subtype="salesforce",
    description=DESCRIPTION,
    url="https://www.salesforce.com",
    warehouse=SalesforceWarehouse,
    flows=(
        Flow(
            Mode.create,
            Entity.profile,
            Direction.inbound,
            format=format_into_hrflow_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.inbound,
            format=format_into_hrflow_profile,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.inbound,
            format=format_into_archive_in_hrlow,
        ),
        Flow(
            Mode.create,
            Entity.profile,
            Direction.outbound,
            format=format_into_salesforce_profile,
        ),
        Flow(
            Mode.update,
            Entity.profile,
            Direction.outbound,
            format=format_into_salesforce_profile,
        ),
        Flow(
            Mode.archive,
            Entity.profile,
            Direction.outbound,
            format=format_into_archive_in_salesforce,
        ),
        Flow(Mode.create, Entity.job, Direction.inbound, format=format_job),
        Flow(Mode.update, Entity.job, Direction.inbound, format=format_job),
        Flow(
            Mode.archive,
            Entity.job,
            Direction.inbound,
            format=format_into_archive_in_hrlow,
        ),
    ),
)
