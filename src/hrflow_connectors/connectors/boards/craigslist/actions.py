from typing import Dict, Any, Iterator, Optional
from pydantic import Field
from selenium import webdriver
from selenium.common.exceptions import WebDriverException
from re import search

from ....core.action import BoardAction
from ....utils.logger import get_logger

logger = get_logger()


class CraigslistFeed(BoardAction):
    archive_deleted_jobs_from_stream: bool = False
    subdomain: str = Field(
        ...,
        description="Subdomain just before `craigslist.org/d/emploi/search/jjj` for example subdomain = `paris` in `https://paris.craigslist.org/d/emploi/search/jjj`, it is also the localisation of the job offers",
    )
    executable_path: Optional[str] = Field(
        None,
        description="A separate executable that Selenium WebDriver used to control Chrome. Make sure you install the chromedriver with the same version as your local Chrome navigator",
    )

    binary_location: Optional[str] = Field(
        None,
        description="Location of the binary chromium, usually in HrFlow workflows it equals `/opt/bin/headless-chromium`",
    )

    jobs_per_page: int = Field(120, const=True)

    @property
    def base_url(self):

        return "https://{}.craigslist.org/d/emploi/search/jjj".format(self.subdomain)

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
            driver = webdriver.Chrome(
                executable_path=self.executable_path, chrome_options=chrome_options
            )

        return driver

    def pull(self) -> Iterator[str]:
        """
        Pull job links

        The role of this function is to scrap and retrieve each job offer link for each job posted

        Returns:
            Iterator[str]: list of scrapped job links
        """
        job_link_list = []

        driver = self.Crawler

        try:
            logger.info(f"Crawler get page : url=`{self.base_url}`")
            driver.get(self.base_url)
        except WebDriverException as e:
            logger.error(f"Fail to get page : url=`{self.base_url}`")
            logger.error(e)
            error_message = f"This site in not available, check if {self.subdomain}.craiglist.org is a valid subdomain"
            raise ConnectionError(error_message)

        driver.maximize_window()
        total_jobs = int(driver.find_element_by_xpath("//*[@class='totalcount']").text)
        logger.info(f"Total jobs to find : {total_jobs}")
        total_pages = total_jobs // self.jobs_per_page + 1
        logger.info(f"Total pages to crawl : {total_pages}")

        for page in range(0, total_pages):
            logger.info(f"Crawling page=`{page + 1}` ...")
            driver.get(self.base_url + "s=%s" % ((page + 1) * self.jobs_per_page))
            # jobs rows
            currant_page_job_list = driver.find_elements_by_xpath(
                "//*[@id='search-results']/li"
            )
            current_page_job_count = len(currant_page_job_list)
            logger.info(f"Number of jobs on this page : {current_page_job_count}")
            # retrieve job link from each job row
            job_link_list += [
                job.find_element_by_tag_name("a").get_attribute("href")
                for job in currant_page_job_list
            ]

        job_link_count = len(job_link_list)
        logger.info(f"Total job links found on all pages : {job_link_count}")
        return job_link_list

    def format(self, job_link: str) -> Dict[str, Any]:
        """
        Format job

        Generates a dictionary of a job attributes, for each job link the function scraps with selenium and parse useful attributes

        Args:
            job_link (str): job link parsed after the function pull is executed

        Returns:
            Dict[str, Any]: Job in the HrFlow job object format
        """
        job = dict()

        logger.debug("Extracting job from a web page")
        driver = self.Crawler

        logger.debug(f"Crawler go to `{job_link}`")
        driver.get(job_link)
        logger.debug(f"Scraping the web page...")

        # job name
        job["name"] = driver.find_element_by_xpath("//*[@id='titletextonly']").text

        # job reference
        # Searching for a match to get the reference to the job
        m = search("7(.+?).html", job_link)
        if m is not None:
            job["reference"] = m.group(1)

        # job url
        job["url"] = job_link

        # job create_at
        job["created_at"] = (
            driver.find_elements_by_xpath("//*[@class='postinginfo reveal']")[0]
            .find_element_by_tag_name("time")
            .get_attribute("datetime")
        )

        # job summary
        job["summary"] = None

        # job location
        job["location"] = dict(text=self.subdomain, lat=None, lng=None)

        # job sections
        job["sections"] = [
            dict(
                name="craigslist_description",
                title="Description",
                description=driver.find_element_by_xpath("//*[@id='postingbody']").text,
            )
        ]

        # tags
        tags = driver.find_elements_by_tag_name("b")
        job["tags"] = [
            dict(name="craigslist_compensation", value=tags[0].text),
            dict(name="craigslist_employment_type", value=tags[1].text),
        ]

        job["ranges_date"] = []
        job["ranges_float"] = []
        job["metadatas"] = []

        logger.debug(f"The web page has been scraped")
        return job