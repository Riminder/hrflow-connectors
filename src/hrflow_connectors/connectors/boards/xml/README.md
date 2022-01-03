# XML Connector
**Many feeds use the XML format. This connector groups together all the `Actions` that process an XML stream to retrieve jobs.**

`XML Stream` :arrow_right: `Hrflow.ai`

## SmartJobs
`XMLBoardAction` retrieves all jobs via an ***XML stream*** API. It adds all these **jobs** to a ***Hrflow.ai Board***.

### Parameters

| Field | Type | Description |
| ----- | ---- | ----------- |
| `logics`  | `List[str]` | Function names to apply as filter before pushing the data. Default value : `[]`        |
| `local_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's local variables. Default value : `None`        |
| `global_scope`  | `Optional[Dict[str, Any]]` | A dictionary containing the current scope's global variables. Default value : `None`       |
| `format_function_name`  | `Optional[str]` | Function name to format job before pushing. Default value : `None`        |
| `hrflow_client` :red_circle: | `hrflow.Hrflow` | Hrflow client instance used to communicate with the Hrflow.ai API        |
| `board_key` :red_circle: | `str` | Board key where the jobs to be added will be stored        |
| `hydrate_with_parsing`  | `bool` | Enrich the job with parsing. Default value : `False`        |
| `archive_deleted_jobs_from_stream`  | `bool` | Archive Board jobs when they are no longer in the incoming job stream. Default value : `True`        |
| `auth` | `Auth` | Auth instance to identify and communicate with the platform. Default value : `NoAuth()`        |
| `xml_stream_url` :red_circle: | `str` | URL to XML Stream    |
| `job_list_xpath` :red_circle: | `str` | XPath pointing to the job list in the XML stream    |

:red_circle: : *required* 

### Example
Let's take the ***Samsic*** job stream as an example of an XML stream.
```python
from hrflow import Hrflow

from hrflow_connectors.core.auth import XSmartTokenAuth
from hrflow_connectors.connectors.boards.smartrecruiters import SmartJobs
from hrflow_connectors.utils.logger import get_logger_with_basic_config

# We add a basic configuration to our logger to see the messages displayed in the standard output
# This is not mandatory. It allows you to see what the connector is doing.
logger = get_logger_with_basic_config()

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

def samsic_format(data):
    job = dict()

    # name
    job["name"] = data.findtext("Title")

    # reference
    job["reference"] = data.findtext("Reference")

    # created_at
    created_at_str = data.findtext("PublicationDateISO")
    created_at_datetime = from_str_to_datetime(created_at_str)
    job["created_at"] = created_at_datetime.isoformat()

    # url
    job["url"] = data.findtext(
        "PostingInstruction/ApplicationMethod/Communication/Text"
    )

    # location
    lat = None
    lng = None
    location_label_list = [
        "Location/CountryCode",
        "Location/City",
        "Location/PostalCode",
        "Location/Address",
    ]
    location_text_list = [
        data.findtext(label, default="") for label in location_label_list
    ]
    location_text = " ".join(location_text_list)

    geojson = dict()
    geojson["city"] = data.findtext("Location/City")
    geojson["country"] = data.findtext("Location/CountryCode")

    postcode = data.findtext("Location/PostalCode")

    geojson["postcode"] = postcode

    job["location"] = dict(lat=lat, lng=lng, text=location_text, geojson=geojson)

    # summary
    job["summary"] = data.findtext("Description")

    # sections
    job["sections"] = []

    def create_section(field_path: str, title: str = None):
        """
        Create a section in job if `field_name` value is not `None`

        Args:
            field_path (str): Field path used to retrieve value in the tree in Samsic. For example : `Organization/Description`
            title (str, optional): Section title. Defaults to None.
        """
        field_name = field_path.replace("/", "-")
        section_name = "samsic_{}".format(field_name)
        section_title = title
        section_description = data.findtext(field_path)
        if section_description is not None:
            section = dict(
                name=section_name,
                title=section_title,
                description=section_description,
            )
            job["sections"].append(section)

    ## Add sections
    create_section("Organization/Description", "Organization description")
    create_section("Profile", "Profile")

    # languages
    job["languages"] = []

    # tags
    job["tags"] = []

    def create_tag(field_path: str):
        """
        Create tag in job if `field_path` is not `None`

        Args:
            field_path (str): Field path used to retrieve value in the tree in Samsic. For example : `Organization/Description`
        """
        field_name = field_path.replace("/", "-")
        tag_name = "crosstalent_{}".format(field_name)
        tag_value = data.findtext(field_path)
        if tag_value is not None:
            tag = dict(name=tag_name, value=tag_value)
            job["tags"].append(tag)

    ## Add tags
    create_tag("Duration")
    create_tag("Organization/Name")
    create_tag("Job")
    create_tag("Contract")
    create_tag("EducationRequirement")
    create_tag("LineOfBusiness")
    create_tag("FieldOfActivite")
    create_tag("DrivingLicence")
    create_tag("RequiredExperience")
    create_tag("Remuneration")

    start_date_str = data.findtext("StartDate")
    start_date_datetime = from_str_to_datetime(start_date_str)
    start_date_tag_value = start_date_datetime.isoformat()
    start_date_tag = dict(name="samsic_StartDate", value=start_date_tag_value)
    job["tags"].append(start_date_tag)

    # metadatas
    job["metadatas"] = []

    return job

xml_stream_url = "https://cv.samsic-emploi.fr/media/flux/jobs.xml"
job_list_xpath = "DataArea"

logger = get_logger_with_basic_config()

action = XMLBoardAction(
    xml_stream_url=xml_stream_url,
    job_list_xpath=job_list_xpath,
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=False,
    archive_deleted_jobs_from_stream=False,
    format_function_name="samsic_format",
    global_scope=globals(),
    local_scope=locals(),
)
action.execute()
```