import datetime
import mimetypes
import typing as t

from hrflow_connectors.connectors.hrflow.warehouse import HrFlowJobWarehouse
from hrflow_connectors.connectors.hrflow.warehouse.profile import (
    HrFlowProfileParsingWarehouse,
)
from hrflow_connectors.connectors.monster.warehouse import (
    MonsterJobWarehouse,
    MonsterProfileWarehouse,
)
from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)


def find_in_tags(tag_list: t.List, keyword: t.AnyStr) -> t.AnyStr:
    return next(iter(tag["value"] for tag in tag_list if tag["name"] == keyword), None)


def find_in_range_list(range_list: t.List, keyword: t.AnyStr) -> t.Tuple:
    return next(
        iter(
            (range["value_min"], range["value_max"])
            for range in range_list
            if range["name"] == keyword
        ),
        None,
    )


def format_created_at(formater: t.Dict, hrflow_job: t.Dict) -> None:
    formater["today"] = hrflow_job["created_at"]


def format_job_informations(formater: t.Dict, hrflow_job: t.Dict) -> None:
    formater["JobTitle"] = hrflow_job["name"]

    JobLevel = find_in_tags(hrflow_job["tags"], "JobLevel")
    formater["JobLevel"] = (
        f'\n        <JobLevel monsterId="{JobLevel}"/>' if JobLevel else ""
    )

    JobType = find_in_tags(hrflow_job["tags"], "JobType")
    formater["JobType"] = (
        f'\n        <JobType monsterId="{JobType}"/>' if JobType else ""
    )

    JobStatus = find_in_tags(hrflow_job["tags"], "JobStatus")
    formater["JobStatus"] = (
        f'\n        <JobStatus monsterId="{JobStatus}"/>' if JobStatus else ""
    )


def format_job_reference(formater: t.Dict, hrflow_job: t.Dict) -> None:
    formater["jobRefCode"] = f'"{hrflow_job["key"]}"'


def format_salary(formater: t.Dict, hrflow_job: t.Dict) -> None:
    Currency = find_in_tags(hrflow_job.get("tags"), "Currency")
    formater["Currency"] = (
        f'\n          <Currency monsterId="{Currency}"/>' if Currency else ""
    )

    formater["SalaryMin"] = ""
    formater["SalaryMax"] = ""
    Salary_range = find_in_range_list(hrflow_job.get("ranges_float"), "Salary")
    if Salary_range:
        SalaryMin = Salary_range[0]
        formater["SalaryMin"] = (
            f"\n          <SalaryMin>{SalaryMin}</SalaryMin>" if SalaryMin else ""
        )
        SalaryMax = Salary_range[1]
        formater["SalaryMax"] = (
            f"\n          <SalaryMax>{SalaryMax}</SalaryMax>" if SalaryMax else ""
        )

    CompensationType = find_in_tags(hrflow_job.get("tags"), "CompensationType")
    formater["CompensationType"] = (
        f'\n          <CompensationType monsterId="{CompensationType}"/>'
        if CompensationType
        else ""
    )


def format_location(formater: t.Dict, hrflow_job: t.Dict) -> None:
    location = hrflow_job["location"]
    StreetAddress = location.get("text")
    formater["StreetAddress"] = StreetAddress if StreetAddress else ""


def format_description(formater: t.Dict, hrflow_job: t.Dict) -> None:
    formater["JobBody"] = hrflow_job["summary"]


def format_duration(formater: t.Dict, hrflow_job: t.Dict) -> None:
    duration = find_in_tags(hrflow_job.get("tags"), "desiredDuration")
    formater["desiredDuration"] = f' desiredDuration="{duration}"' if duration else ""


def format_autorefresh(formater: t.Dict, hrflow_job: t.Dict) -> None:
    Autorefresh = find_in_tags(hrflow_job.get("tags"), "Autorefresh")
    formater["Autorefresh"] = (
        """\n            <Autorefresh desired="true">
            <Frequency>{frequency}</Frequency>
        </Autorefresh>""".format(
            frequency=Autorefresh
        )
        if Autorefresh
        else ""
    )


def format_careeradnetwork(formater: t.Dict, hrflow_job: t.Dict) -> None:
    CareerAdNetwork = find_in_tags(hrflow_job.get("tags"), "CareerAdNetwork")
    formater["CareerAdNetwork"] = (
        """\n            <CareerAdNetwork desired="true">
            <Duration>{frequency}</Duration>
        </CareerAdNetwork>""".format(
            frequency=CareerAdNetwork
        )
        if CareerAdNetwork
        else ""
    )


def format_jobcategory(formater: t.Dict, hrflow_job: t.Dict) -> None:
    JobCategory = find_in_tags(hrflow_job.get("tags"), "JobCategory")
    formater["JobCategory"] = JobCategory if JobCategory else "11"


def format_joboccupation(formater: t.Dict, hrflow_job: t.Dict) -> None:
    JobOccupation = find_in_tags(hrflow_job.get("tags"), "JobOccupation")
    formater["JobOccupation"] = JobOccupation if JobOccupation else "11892"


def format_industries(formater: t.Dict, hrflow_job: t.Dict) -> None:
    Industry = find_in_tags(hrflow_job.get("tags"), "Industry")
    formater["Industry"] = (
        """\n          <Industries>
        <Industry>
            <IndustryName monsterId="{IndustryName}"/>
        </Industry>
        </Industries>""".format(
            IndustryName=Industry
        )
        if Industry
        else ""
    )


def format_job(data: t.Dict) -> str:
    xml_job_str = """<?xml version="1.0" encoding="UTF-8"?>
<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">
  <SOAP-ENV:Header>
    <mh:MonsterHeader xmlns:mh="http://schemas.monster.com/MonsterHeader">
      <mh:MessageData>
        <mh:MessageId>Creating a job</mh:MessageId>
        <mh:Timestamp>{today}</mh:Timestamp>
      </mh:MessageData>
    </mh:MonsterHeader>
    <wsse:Security xmlns:wsse="http://schemas.xmlsoap.org/ws/2002/04/secext">
      <wsse:UsernameToken>
        <wsse:Username></wsse:Username>
        <wsse:Password></wsse:Password>
      </wsse:UsernameToken>
    </wsse:Security>
  </SOAP-ENV:Header>
  <SOAP-ENV:Body>
    <Job jobRefCode={jobRefCode} jobAction="addOrUpdate"
    inventoryType="transactional"
    xmlns="http://schemas.monster.com/Monster"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xsi:schemaLocation="http://schemas.monster.com/Monster
    http://schemas.monster.com/Current/xsd/Monster.xsd">
      <RecruiterReference>
        <UserName></UserName>
      </RecruiterReference>
      <JobInformation>
        <JobTitle>{JobTitle}</JobTitle>{JobLevel}{JobType}{JobStatus}
        <Salary>{Currency}{SalaryMin}{SalaryMax}{CompensationType}
        </Salary>
        <PhysicalAddress>
          <StreetAddress>{StreetAddress}</StreetAddress>
        </PhysicalAddress>
        <HideCompanyInfo>false</HideCompanyInfo>
        <JobBody>{JobBody}
        </JobBody>
        <ApplyWithMonster>
          <DeliveryMethod monsterId="5" />
          <DeliveryFormat monsterId="1" />
          <VendorText>This apply comes from a job on Monster</VendorText>
          <PostURL>https://webhook.site/ef84319c-4bc4-4c56-914f-3ef557ccc6db</PostURL>
          <ApiKey></ApiKey>
        </ApplyWithMonster>
      </JobInformation>
      <JobPostings>
        <JobPosting{desiredDuration} bold="true">
         <InventoryPreference subscription="false">{Autorefresh}{CareerAdNetwork}
           </InventoryPreference>
          <Location>
            <City>Boston</City>
            <State>MA</State>
            <CountryCode>US</CountryCode>
            <PostalCode>02125</PostalCode>
          </Location>
          <JobCategory monsterId="{JobCategory}"/>
          <JobOccupations>
            <JobOccupation monsterId="{JobOccupation}"/>
          </JobOccupations>
          <BoardName monsterId="178"/>
          <DisplayTemplate monsterId="1"/>{Industry}
        </JobPosting>
      </JobPostings>
    </Job>
  </SOAP-ENV:Body>
</SOAP-ENV:Envelope>"""

    creation_pipeline = [
        format_job_informations,
        format_created_at,
        format_job_reference,
        format_salary,
        format_location,
        format_description,
        format_duration,
        format_autorefresh,
        format_careeradnetwork,
        format_jobcategory,
        format_joboccupation,
        format_industries,
    ]

    formater = dict()
    for function in creation_pipeline:
        function(formater, data)
    job = xml_job_str.format(**formater)
    return job.encode("utf-8")


def get_content_type_from_extension(extension: str) -> str:
    mimetypes.init()
    return mimetypes.types_map[extension]


def format_profile(Monster_profile: t.Dict) -> t.Dict:
    """
    Format the input data into a push-ready data schema
    Args:
        request (Dict[str, Any]): body we want to adapt to the output format
    Returns:
        Dict[str, Any]: parameters to put in the parsing endpoint
    """

    def get_binary_resume(FileContents):
        byte_array = bytearray(FileContents)
        binary_resume = bytes(byte_array)
        return binary_resume

    hrflow_tags = [{"name": "JobRefID", "value": Monster_profile["JobRefID"]}]

    output_data = {
        "resume": {
            "raw": get_binary_resume(Monster_profile["FileContents"]),
            "content_type": get_content_type_from_extension(Monster_profile["FileExt"]),
        },
        "reference": Monster_profile["ResumeValue"],
        "tags": hrflow_tags,
        "metadatas": [],
        "created_at": datetime.datetime.now().isoformat(),
    }
    return output_data


def profile_new_parser(event: t.Dict) -> t.Dict:
    return dict(profile=event)


DESCRIPTION = (
    "facilitate those who share our singular goal to help seekers and employers Find"
    " Better. By partnering with us, your business can gain incredible value by"
    " leveraging Monster's power to reach and connect, for solutions from posting and"
    " applying to jobs to searching for resumes and reaching local talent."
)

Monster = Connector(
    name="Monster",
    description=DESCRIPTION,
    url="https://www.monster.com/",
    actions=[
        ConnectorAction(
            name="push_jobs",
            trigger_type=WorkflowType.pull,
            description=(
                "push a job from  ***Hrflow*** to ***Monster*** API. To see job pushed,"
                " go to http://jobview.monster.com/getjob.aspx?jobid=xxx with xxx is"
                " jobposting id, findable in response. Manual Test for push here:"
                " https://integrations.monster.com/Toolkit/"
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteJobsActionParameters", format=format_job
            ),
            origin=HrFlowJobWarehouse,
            target=MonsterJobWarehouse,
        ),
        ConnectorAction(
            name="catch_profile",
            trigger_type=WorkflowType.catch,
            description="catches a Monster profile to Hrflow.ai",
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters",
                format=format_profile,
                event_parser=profile_new_parser,
            ),
            origin=MonsterProfileWarehouse,
            target=HrFlowProfileParsingWarehouse,
        ),
    ],
)
