import typing as t
from hrflow_connectors.connectors.hrflow.warehouse import (
    HrFlowJobWarehouse,
)
from hrflow_connectors.connectors.hrflow.warehouse.profile import HrFlowProfileParsingWarehouse
from hrflow_connectors.connectors.monster.warehouse import MonsterJobWarehouse, MonsterProfileWarehouse

from hrflow_connectors.core import (
    BaseActionParameters,
    Connector,
    ConnectorAction,
    WorkflowType,
)
#https://webhook.site/360f0dec-c55b-4a29-92d9-210b70d14ad3

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

    def find_in_tags(tag_list, keyword):
        if tag_list == None:
            return None
        for tag in tag_list:
            if tag["name"] == keyword:
                return tag["value"]

    def find_in_range_list(range_list, keyword):
        if range_list == None:
            return None
        for range_ in range_list:
            if range_["name"] == keyword:
                return (range_["value_min"], range_["value_max"])

    def format_created_at(formater, hrflow_job):
        formater["today"] = hrflow_job["created_at"]

    def format_job_informations(formater, hrflow_job):
        formater["JobTitle"] = hrflow_job["name"]
        formater["JobLevel"] = ""
        formater["JobType"] = ""
        formater["JobStatus"] = ""

        JobLevel = find_in_tags(hrflow_job["tags"], "JobLevel")
        if JobLevel != None:
            formater["JobLevel"] = f'\n        <JobLevel monsterId="{JobLevel}"/>'

        JobType = find_in_tags(hrflow_job["tags"], "JobType")
        if JobType != None:
            formater["JobType"] = f'\n        <JobType monsterId="{JobType}"/>'

        JobStatus = find_in_tags(hrflow_job["tags"], "JobStatus")
        if JobStatus != None:
            formater[
                "JobStatus"
            ] = f'\n        <JobStatus monsterId="{JobStatus}"/>'

    def format_job_reference(formater, hrflow_job):
        formater["jobRefCode"] = f'"{hrflow_job["key"]}"'

    def format_salary(formater, hrflow_job):
        formater["Currency"] = ""
        formater["SalaryMin"] = ""
        formater["SalaryMax"] = ""
        formater["CompensationType"] = ""

        Currency = find_in_tags(hrflow_job.get("tags"), "Currency")
        if Currency != None:
            formater["Currency"] = f'\n          <Currency monsterId="{Currency}"/>'

        Salary_range = find_in_range_list(hrflow_job.get("ranges_float"), "Salary")

        if Salary_range != None:
            SalaryMin = Salary_range[0]
            formater[
                "SalaryMin"
            ] = f"\n          <SalaryMin>{SalaryMin}</SalaryMin>"
            SalaryMax = Salary_range[1]
            formater[
                "SalaryMax"
            ] = f"\n          <SalaryMax>{SalaryMax}</SalaryMax>"

        CompensationType = find_in_tags(hrflow_job.get("tags"), "CompensationType")
        if CompensationType != None:
            formater[
                "CompensationType"
            ] = f'\n          <CompensationType monsterId="{CompensationType}"/>'

    def format_location(formater, hrflow_job):
        formater["StreetAddress"] = ""

        location = hrflow_job["location"]
        StreetAddress = location.get("text")

        if StreetAddress != None:
            formater["StreetAddress"] = StreetAddress

    def format_description(formater, hrflow_job):
            formater["JobBody"] = hrflow_job["summary"]

    def format_duration(formater, hrflow_job):
        formater["desiredDuration"] = ""
        duration = find_in_tags(hrflow_job.get("tags"), "desiredDuration")
        if duration != None:
            formater["desiredDuration"] = f' desiredDuration="{duration}"'

    def format_autorefresh(formater, hrflow_job):
        formater["Autorefresh"] = ""
        Autorefresh = find_in_tags(hrflow_job.get("tags"), "Autorefresh")
        if Autorefresh != None:
            formater[
                "Autorefresh"
            ] = """\n            <Autorefresh desired="true">
                    <Frequency>{frequency}</Frequency>
                </Autorefresh>""".format(
                frequency=Autorefresh
            )

    def format_careeradnetwork(formater, hrflow_job):
        formater["CareerAdNetwork"] = ""
        CareerAdNetwork = find_in_tags(hrflow_job.get("tags"), "CareerAdNetwork")
        if CareerAdNetwork != None:
            formater[
                "CareerAdNetwork"
            ] = """\n            <CareerAdNetwork desired="true">
                    <Duration>{frequency}</Duration>
                </CareerAdNetwork>""".format(
                frequency=CareerAdNetwork
            )

    def format_jobcategory(formater, hrflow_job):
        formater["JobCategory"] = "11"
        JobCategory = find_in_tags(hrflow_job.get("tags"), "JobCategory")
        if JobCategory != None:
            formater["JobCategory"] = JobCategory

    def format_joboccupation(formater, hrflow_job):
        formater["JobOccupation"] = "11892"
        JobOccupation = find_in_tags(hrflow_job.get("tags"), "JobOccupation")
        if JobOccupation != None:
            formater["JobOccupation"] = JobOccupation

    def format_industries(formater, hrflow_job):
        formater["Industry"] = ""
        Industry = find_in_tags(hrflow_job.get("tags"), "Industry")
        if Industry != None:
            formater[
                "Industry"
            ] = """\n          <Industries>
                <Industry>
                    <IndustryName monsterId="{IndustryName}"/>
                </Industry>
                </Industries>""".format(
                IndustryName=Industry
            )

    #def format_credentials(formater, hrflow_job):
    #    formater["username"] = "{username}"
    #    formater["password"] = "{password}"

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
    print(dict(Monster_profile))
    raise Exception(Monster_profile)
    hrflow_tags = [{"name": "JobRefID", "value": Monster_profile["JobRefID"]}]

    output_data = {
        "profile_file": get_binary_resume(Monster_profile["FileContents"]),
        "tags": hrflow_tags,
    }
    return output_data



DESCRIPTION = (
    "facilitate those who share our singular goal to help seekers and employers"
    " Find Better. By partnering with us, your business can gain incredible value by"
    " leveraging Monster's power to reach and connect, for solutions from posting and applying"
    " to jobs to searching for resumes and reaching local talent." 
)
def profile_new_parser(
    event: t.Dict
):
    return dict(profile=event)

Monster = Connector(
    name="Monster",
    description=DESCRIPTION,
    url="https://www.monster.com/",
    actions=[
        ConnectorAction(
            name="push_jobs",
            trigger_type=WorkflowType.pull,
            description=(
                "push a job from  ***Hrflow***"
                " to ***Monster*** API. To see job pushed, go to http://jobview.monster.com/getjob.aspx?jobid=xxx "
                "with xxx jobposting id findable in response. Test manual push here: https://integrations.monster.com/Toolkit/"
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
            description=(
                "catches a Monster profile to Hrflow.ai."
            ),
            parameters=BaseActionParameters.with_defaults(
                "WriteProfileActionParameters", format=format_profile,
                event_parser=profile_new_parser
            ),
            origin=MonsterProfileWarehouse,
            target=HrFlowProfileParsingWarehouse,
        ),
    ],
)
