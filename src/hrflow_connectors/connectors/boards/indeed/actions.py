from typing import Iterator, Dict, Any
from ....core.action import BoardAction
import re
from pydantic import Field
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from time import sleep
from hrflow import Hrflow
from selenium import webdriver
from enum import Enum
from selenium.common.exceptions import NoSuchElementException
from typing import Any


class Crawler:

    """
    Selenium Crawler Class

    """

    def __init__(self):
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
        chrome_options.binary_location = "/opt/bin/headless-chromium"
        self._driver = webdriver.Chrome(chrome_options=chrome_options)

    def get_driver(self):

        return self._driver


class GetAllJobs(BoardAction, Crawler):

    job_search: str = Field(..., description="Search Job offers in 'fr.indeed.com'")
    job_location: str = Field(..., description="Location of the job offers")
    job_board_url: str = Field(..., description="https://fr.indeed.com")
    limit: int = Field(
        ...,
        description=" limit of jobs extracted on page, usually on 'indeed.com', the number of offers per page is 15",
    )
    limit_extract: int = Field(..., description=" limit of pages you want to extract")

    def url_base(self, pagination: int) -> str:
        """
        Params:
            pagination (int):  Page number

        Desc :
            Generate a url.
        Returns :
            string url.
        """

        return "https://fr.indeed.com/emplois?q={}&l={}&limit={}&start={}".format(
            self.job_search, self.job_location, self.limit, pagination
        )

    def format(self, joblink: str) -> Dict[str, Any]:
        """
        parameter: joblink parsed after the function pull is executed.

        Description: generates a dictionary of a job attributes, for each job link the function scraps with selenium and parse useful attributes.

        returns: a job in the HrFlow job object format
        """
        job = dict()
        driver = Crawler.get_driver()
        driver.get(joblink)

        # name
        try:
            job["name"] = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[2]/div[1]/div[1]/h1"
            ).text

        except NoSuchElementException:

            job["name"] = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[3]/div[1]/div[1]/h1"
            ).text
            pass

        # reference
        m = re.search("jk=(.+?)&tk", joblink)
        job["reference"] = m.group(1)

        # created_at, updated_at : isn't shown on ideed, TODO : convert "il y a n jours" into date time
        job["created_at"] = None
        job["updated_at"] = None

        # location
        try:
            text = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[3]/div[1]/div[2]/div/div/div[2]"
            ).text

        except NoSuchElementException:

            text = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[2]/div[1]/div[2]/div/div/div[2]"
            ).text

        job["location"] = dict(text=text, lat=None, lng=None)

        # url
        job["url"] = joblink

        # summary
        job["summary"] = driver.find_element_by_id("jobDescriptionText").text

        # description
        description = driver.find_element_by_id("descriptionText").text

        job["sections"] = list(
            dict(name="description", title="description", description=description)
        )

        # compensation
        try:
            salary = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[2]/div[2]/span[1]"
            ).text

            if (
                "Stage" in salary.split()
            ):  # Mandatory to be sure that we are parsing a salary and not a job type because of indeed web design structure
                salary = None

            if (
                "Apprentissage" in salary.split()
            ):  # Mandatory to be sure that we are parsing a salary and not a job type because of indeed web design structure

                salary = None

            if (
                "CDI" in salary.split()
            ):  # Mandatory to be sure that we are parsing a salary and not a job type because of indeed web design structure

                salary = None

            if (
                "Alternance" in salary.split()
            ):  # Mandatory to be sure that we are parsing a salary and not a job type because of indeed web design structure

                salary = None

            if (
                "Temps plein" in salary.split()
            ):  # Mandatory to be sure that we are parsing a salary and not a job type because of indeed web design structure

                salary = None

        except NoSuchElementException:
            salary = None

        # employment_type
        try:
            jobType = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[2]/div[2]/span[2]/"
            ).text

        except NoSuchElementException:
            pass

        try:
            jobType = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[2]/div[2]"
            ).text

        except NoSuchElementException:
            pass

        try:
            jobType = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[3]/div[2]/span"
            ).text

        except NoSuchElementException:
            pass

        try:
            jobType = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[2]/div[2]/span"
            ).text

        except NoSuchElementException:
            pass

        if "par" in jobType.strip():
            jobType = None

        job["tags"] = list(
            dict(name="indeed_compensantion", value=salary),
            dict(name="indeed_employment_type", value=jobType),
        )

        job["ranges_date"] = list()
        job["ranges_float"] = list()
        job["metadatas"] = list()

        return job

    def pull(self) -> list:

        jobsLinks = []
        driver = Crawler.get_driver()
        driver.get(self.job_board_url)
        # Find the text box and enter the type of job the user entered earlier
        search = driver.find_element_by_id("text-input-what")
        search.send_keys(self.job_search)
        search.send_keys(Keys.RETURN)

        try:
            # Get total number of job inside a string.
            total_job_s = driver.find_element_by_xpath(
                "//div[@id='searchCountPages']"
            ).text
        except NoSuchElementException:
            pass

        total_job_s = total_job_s.split()
        start = total_job_s.index("de")
        end = total_job_s.index("emplois")

        total_jobs = int("".join([total_job_s[i] for i in range(start + 1, end)]))
        count_jobs = self.limit  # max 15 jobs par parge
        total_page = int(total_jobs / count_jobs)

        for page in range(0, total_page):
            if page == self.limit_extract:
                break
            page_url = self.url_base(pagination=(page * count_jobs))
            driver.get(page_url)
            sleep(3)

            try:
                jobCards = WebDriverWait(driver, 10).until(
                    EC.presence_of_element_located((By.ID, "mosaic-provider-jobcards"))
                )

                elements = jobCards.find_elements_by_class_name("tapItem")
                for x in elements:

                    link = x.get_attribute("href")
                    jobsLinks.append(link)

            finally:
                driver.quit()
                pass

        return jobsLinks
