from typing import Dict, Any, Iterator, Optional
from ....core.action import BoardAction
from pydantic import Field
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from time import sleep
from selenium.common.exceptions import NoSuchElementException


class JobsBuilder(BoardAction):

    domain: str = Field(
        ...,
        description="domain just after `https://www.careerbuilder.` for example domain = `fr` in `https://www.careerbuilder.fr`",
    )

    job_search: str = Field(
        ...,
        description="Name of the job position we want to search offers for in 'careerbuilder'",
    )
    job_location: str = Field(..., description="Location of the job offers")

    executable_path: Optional[str] = Field(
        None,
        description="A separate executable that Selenium WebDriver used to control Chrome. Make sure you install the chromedriver with the same version as your local Chrome navigator",
    )

    binary_location: Optional[str] = Field(
        None,
        description="Location of the binary chromium, usually in HrFlow workflows it equals `/opt/bin/headless-chromium`",
    )

    @property
    def base_url(self) -> str:
        return "https://www.careerbuilder.{}".format(self.domain)

    @property
    def Crawler(self):
        """
        Selenium Crawler function
        """
        chrome_options = webdriver.ChromeOptions()
        chrome_options.add_argument("--headless")
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument("--disable-dev-shm-usage")
        chrome_options.add_argument("--disable-gpu")
        chrome_options.add_argument("--window-size=1280x1696")
        chrome_options.add_argument("--hide-scrollbars")
        chrome_options.add_argument("--enable-logging")
        chrome_options.add_argument("--log-level=0")
        chrome_options.add_argument("--v=99")
        chrome_options.add_argument("--single-process")
        chrome_options.add_argument("--ignore-certificate-errors")

        if self.binary_location is not None:
            chrome_options.binary_location = self.binary_location

        if self.executable_path is None:
            driver = webdriver.Chrome(chrome_options)
        else:
            driver = webdriver.Chrome(
                executable_path=self.executable_path, chrome_options=chrome_options
            )

        return driver

    def pull(self) -> Iterator[str]:
        """
        Pull, the role of this function is to interact with careerbuilder webpage, click buttons and search offers based on job title and location.
        it scraps all the job cards shown on the page, and for each job card it retrieves its individual link

        Returns:
          Iterator[str]:  list of scrapped job links

        """
        driver = self.Crawler
        driver.get(self.base_url)
        # search job search and job location cases
        search_key = driver.find_elements_by_class_name(
            "autocomplete-accessibility-input"
        )
        search_key[0].send_keys(self.job_search)
        search_key[1].send_keys(self.job_location)
        # click on the search button after sending our keys
        driver.find_element_by_class_name("submit-text").click()
        try:  # in case there are more jobs to load and scroll down to catch
            driver.find_element_by_id("load_more_jobs").click()
        # if there is no need to load more jobs on the page we pass the exception
        except NoSuchElementException:
            pass
        # get all the jobcards available on the page
        jobs = driver.find_elements_by_xpath(
            "//*[@class='data-results-content-parent relative']"
        )
        # get the list of the links of job cards
        job_link_list = [
            job.find_element_by_tag_name("a").get_attribute("href") for job in jobs
        ]

        return job_link_list

    def format(self, data: str) -> Dict[str, Any]:
        """
        Format, generates a dictionary of a job attributes, for each job link the function scraps with selenium and parse useful attributes.

        Args:
            data (str): job_link parsed after the function pull is executed. each job link is retrieved from the job_links_list scrapped in the pull function

        Returns:
            Dict[str, Any]: a job in the HrFlow job object format
        """
        driver = self.Crawler
        driver.get(data)
        job = dict()
        # name
        job["name"] = driver.find_element_by_class_name("jdp_title_header")
        # location
        job["location"] = driver.find_elements_by_xpath('//*[@id="jdp-data"]//span')[
            1
        ].text
        # JobType
        employment_type = driver.find_elements_by_xpath('//*[@id="jdp-data"]//span')[
            2
        ].text
        # salary
        salary = driver.find_element_by_xpath('//*[@id="cb-salcom-info"]/div').text
        job["tags"] = [
            dict(name="career_builder_compensation", value=salary),
            dict(name="career_builder_employment_type", value=employment_type),
        ]
        # description
        description = driver.find_element_by_class_name("jdp-description-details").text
        job["sections"] = [
            {
                "name": "career_builder_description",
                "title": "description",
                "description": description,
            }
        ]
        job["summary"] = None
        job["reference"] = None
        job["metadata"] = []
        return job
