import responses

from hrflow_connectors import XML
from hrflow_connectors.connectors.xml.actions import PullJobsAction
from hrflow_connectors.utils.datetime_converter import from_str_to_datetime


def test_PullJobsAction_from_samsic_xml_stream(logger, hrflow_client):
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
        ## TODO ajouter utils de Zipcode -> Coords
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
    XML.pull_jobs(
        xml_stream_url=xml_stream_url,
        job_list_xpath=job_list_xpath,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="ebf489eff6bef0e95ca03eb0d6ed8f8e030a634f",
        hydrate_with_parsing=False,
        archive_deleted_jobs_from_stream=True,
        format_function_name="samsic_format",
        global_scope=globals(),
        local_scope=locals(),
    )


@responses.activate
def test_PullJobsAction_pull_generic_xml_stream(logger, hrflow_client):
    xml_stream_url = "https://test.test/job/xml_stream"

    xml_stream_str = """<?xml version="1.0" encoding="UTF-8"?>
<all>
    <meta>
        <update>2021-11-09T15:00:00.999651</update>
        <version>42</version>
    </meta>
    <board>
        <jobs>
            <job data-id="1">
                <name>Data scientist</name>
                <reference>ds42</reference>
            </job>
            <job data-id="2">
                <name>Software Engineer</name>
                <reference>se18</reference>
            </job>
            <job data-id="3">
                <name>Painter</name>
                <reference>pt56</reference>
            </job>
        </jobs>
    </board>
</all>"""

    responses.add(responses.GET, xml_stream_url, status=200, body=xml_stream_str)

    job_list_xpath = "board/jobs"
    action = PullJobsAction(
        xml_stream_url=xml_stream_url,
        job_list_xpath=job_list_xpath,
        hrflow_client=hrflow_client("dev-demo"),
        board_key="abc",
        hydrate_with_parsing=False,
        archive_deleted_jobs_from_stream=False,
    )
    xml_job_node_list = action.pull()

    assert len(xml_job_node_list) == 3
    job_node_1, job_node_2, job_node_3 = xml_job_node_list

    name_1, reference_1 = list(job_node_1)
    assert job_node_1.attrib["data-id"] == "1"
    assert name_1.text == "Data scientist"
    assert reference_1.text == "ds42"

    name_2, reference_2 = list(job_node_2)
    assert job_node_2.attrib["data-id"] == "2"
    assert name_2.text == "Software Engineer"
    assert reference_2.text == "se18"

    name_3, reference_3 = list(job_node_3)
    assert job_node_3.attrib["data-id"] == "3"
    assert name_3.text == "Painter"
    assert reference_3.text == "pt56"
