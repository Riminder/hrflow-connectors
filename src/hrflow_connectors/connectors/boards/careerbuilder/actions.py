from typing import Dict, Any, Iterator, Optional
from ....core.action import BoardAction
from ....utils.logger import get_logger
from pydantic import Field
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import (
    NoSuchElementException,
    WebDriverException,
    ElementNotInteractableException,
    StaleElementReferenceException,
)


logger = get_logger()


class CareerBuilderFeed(BoardAction):

    archive_deleted_jobs_from_stream: bool = False
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
    maximum_page_num: Optional[int] = Field(
        None,
        description="Maximum `number of pages` you want to scroll, `careerbuilder` website pagination is designed as an infinite scroller loading",
    )
    sort_by_date: bool = Field(
        False,
        description="by default, results are sorted by relevancy which gives the best results but may cause getting old results that are no longer available which provokes `selenium Timeout erros` in `format: driver.get('job_link')`. To sort by date, switch this variable to True",
    )

    @property
    def base_url(self) -> str:
        return "https://www.careerbuilder.{}".format(self.domain)

    @property
    def Crawler(self):
        """
        Selenium Crawler function
        """
        logger.info("Configuring Chrome Webdriver...")
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
            logger.info(f"Chrome binary location : {self.binary_location}")
            chrome_options.binary_location = self.binary_location

        if self.executable_path is None:
            driver = webdriver.Chrome(chrome_options)
        else:
            logger.info(f"Webdriver executable path : {self.executable_path}")
            driver = webdriver.Chrome(
                executable_path=self.executable_path, chrome_options=chrome_options
            )
        return driver

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull, the role of this function is to interact with careerbuilder webpage, click buttons and search offers based on job title and location.
        it scraps all the job cards shown on the page, and for each job card it retrieves its individual link and reference

        Returns:
          Iterator[Dict[str, Any]]:  list of dictionaries {job_ref:job_link}
        """
        driver = self.Crawler
        try:
            logger.info(f"Crawler get page : url=`{self.base_url}`")
            driver.get(self.base_url)
        except WebDriverException as e:
            logger.error(f"Failed to get page : url=`{self.base_url}`")
            logger.error(e)
            error_message = f"This website in not available, check if `carrerbuilder.{self.domain}` is a valid domain"
            raise ConnectionError(error_message)
        # search job search and job location cases
        search_key = driver.find_elements_by_class_name(
            "autocomplete-accessibility-input"
        )
        logger.info(f"Crawler send a query `{self.job_search}`")
        search_key[0].send_keys(self.job_search)
        logger.info(f"Crawler send a query `{self.job_location}`")
        search_key[1].send_keys(self.job_location)
        # click on the search button after sending our keys
        driver.find_element_by_class_name("submit-text").click()

        if self.sort_by_date:
            try:
                logger.info(f"sorting resuts by date: {self.sort_by_date}")
                driver.find_element_by_name("date").click()

            except NoSuchElementException:  # case when there are no results, we pass to make it more explicit down
                pass
        sleep(3)
        try:  # In case there are more results than those shown so we need to load more jobs on the page
            logger.info(
                "Crawler loading more jobs if there are more results than shown initially"
            )
            load_more_jobs = driver.find_element_by_css_selector(
                "#load_more_jobs > button"
            )
            page_num = 1  # first page of results
            while load_more_jobs:
                if (
                    page_num == self.maximum_page_num
                ):  # if page_num reaches limit set by user
                    logger.info(
                        f"reached maximum page limit set by customer {self.maximum_page_num}"
                    )
                    break
                try:
                    sleep(4)  # give the driver time to find element and click
                    load_more_jobs.click()
                    page_num += 1
                    logger.debug(f"loading page number: {page_num}")

                except (
                    ElementNotInteractableException,
                    StaleElementReferenceException,
                ):  # i.e (element is present in the DOM but interactions with that element will hit another element do to paint order, Thrown when a reference to an element is now “stale”) by order of exceptions
                    load_more_jobs = False
        except NoSuchElementException:  # Except if the driver don't need to scroll down to get all jobs we pass
            logger.info("There is one or no page of results")
            pass

        # get all job cards web elements available on the page
        logger.info("Getting all job cards")
        sleep(
            5
        )  # give the driver time to get all jobs, especially when there are thousands on one page
        jobs = driver.find_elements_by_xpath(
            "//*[@class='data-results-content block job-listing-item']"
        )
        if len(jobs) == 0:
            error_message = f"Could not find any matching jobs on the page, check that your search keys: `{self.job_search}`, `{self.job_location}` are valid!"
            raise NoSuchElementException(error_message)

        elements_count = len(jobs)
        logger.info(f"Number of jobs found for this search : {elements_count}")

        # get the list of the links and references of job cards
        # job ref                     :            job link
        job_ref_link_list = [
            {job.get_attribute("data-job-did"): job.get_attribute("href")}
            for job in jobs
        ]

        job_refs_count = len(job_ref_link_list)
        job_link_count = len(job_ref_link_list)
        logger.info(f"Number of total job references found : {job_refs_count}")
        logger.info(f"Number of total job links found : {job_link_count}")

        return job_ref_link_list

    def format(self, data: Dict[str, str]) -> Dict[str, Any]:
        """
        Format, generates a dictionary of a job attributes, for each job link(data value) the function scraps with selenium and parse useful attributes.

        Args:
            data Dict[str, str]: a dictionary with job reference as key and job link as value generated after the function pull is executed.

        Returns:
            Dict[str, Any]: a job in the HrFlow job object format
        """
        # getting job_ref and job_link from previously pulled data
        ((job_ref, job_link),) = data.items()

        job = dict()

        logger.debug("Extracting job from its link web page")
        driver = self.Crawler
        logger.debug(f"Crawling `{job_link}`")
        driver.get(job_link)
        logger.debug(f"Scraping the web page...")

        job["url"] = job_link
        # name
        job["name"] = driver.find_element_by_class_name("jdp_title_header").text
        # job_details : CompanyName|Location|EmploymentType, a list of three elements can be [name,"",Type] and so on...
        job_detail = driver.find_elements_by_xpath(
            '//*[@id="jdp-data"]//span'
        )  # can be a list of two or three elements
        # location
        location = job_detail[1].text
        job["location"] = dict(text=location, lat=None, lng=None)
        # JobType
        if len(job_detail) == 3:
            employment_type = job_detail[2].text
        else:
            employment_type = None
        # CompanyName
        company_name = job_detail[0].text
        # salary
        salary = driver.find_element_by_xpath('//*[@id="cb-salcom-info"]/div').text
        job["tags"] = [
            dict(name="career_builder_companyName", value=company_name),
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
        job["reference"] = job_ref
        job["metadata"] = []
        driver.quit()
        logger.debug(f"The web page has been scraped")

        return job
