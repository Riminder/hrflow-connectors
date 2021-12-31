from typing import Dict, Any, Iterator, Optional
import re
from pydantic import Field
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from time import sleep
from selenium.common.exceptions import NoSuchElementException


from ....core.action import BoardAction
from ....utils.logger import get_logger


logger = get_logger()


class IndeedFeed(BoardAction):
    archive_deleted_jobs_from_stream: bool = False
    subdomain: str = Field(
        ...,
        description="Subdomain just before `indeed.com` for example subdomain =`fr` in `https:/fr.indeed.com`",
    )
    job_search: str = Field(
        ...,
        description="Name of the job position we want to search offers in `fr.indeed.com`",
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

    max_page: Optional[int] = Field(
        None, description="Maximum number of pages to search"
    )

    job_per_page: int = Field(15, const=True)

    @property
    def url_base(self) -> str:
        return "https:/{}.indeed.com".format(self.subdomain)

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

    def path(self, job_offset: int) -> str:
        """
        Get the generated path

        This function generates the path and pagination of the job offers search

        Args:
            job_offset (int): Job offset. For example job_offset = 10 if we want to start from page 2

        Returns:
            str: return the generated path. For example it returns `/emplois?q=data%20scientist&l=paris&start=10` if we are looking for Data Scientist offers in Paris in page 2
        """
        return "/emplois?q={query}&l={location}&start={start}".format(
            query=self.job_search,
            location=self.job_location,
            start=job_offset,
        )

    def pull(self) -> Iterator[str]:
        """pull [the role of this function is to interact with indeed, click buttons and search offers based on job title and location.
        for each page we scrap all the job cards shown (usually 15 per page), and for each job card it retrieves its individual link]

        Returns:
            Iterator[str]: [list of scrapped jobLinks]
        """
        job_links_list = []
        driver = self.Crawler

        logger.info(f"Crawler go to `{self.url_base}`")
        driver.get(self.url_base)
        # Find the text box and enter the type of job the user wants to get offers data
        search = driver.find_element_by_id("text-input-what")
        logger.info(f"Crawler send a query `{self.job_search}`")
        search.send_keys(self.job_search)
        search.send_keys(Keys.RETURN)

        # Get the search count str for example 'Page 1 de 993 emplois'
        search_count_str = driver.find_element_by_id("searchCountPages").text
        # retrieve the number of total related job offers from string 'for example from 'Page 1 de 993 emplois' we get job_search_count = 993'
        search_count = (
            search_count_str.split()
        )  # split the string in a list of strings ['1', 'de', '993', 'emplois']
        start = search_count.index("de")
        end = search_count.index(
            "emplois"
        )  # find the words that surround the total number of jobs
        job_search_count = int(
            "".join([search_count[i] for i in range(start + 1, end)])
        )  # group different parts of the number together for example if it is 'Page 1 de 1 993 emplois' we get 1993

        logger.info(f"Number of jobs found for this query : {job_search_count}")
        # If there is only one job offer result -corresponding to 'emploi'- or none a value error is raised, it is a rare case and the custom should make sure to type in a real job name.

        total_page = int(
            job_search_count / self.job_per_page
        )  # for example for a total of 993 and a count of 15 we loop over 66 pages
        logger.info(f"Number of pages found for this query : {total_page}")

        if self.max_page is not None:
            logger.info(f"Max pages requests by the user : {self.max_page}")
            total_page = self.max_page

        for page in range(0, total_page):
            logger.info(f"Scraping the page `{page}` to get the list of jobs")
            page_url = self.url_base + self.path(job_offset=self.job_per_page * page)
            logger.info(f"Crawler go to `{page_url}`")
            driver.get(page_url)
            logger.info(f"Crawler sleep 3 secondes")
            sleep(3)
            try:  # get jobCards inside of a page
                logger.info("Getting mosaic of jobs")
                jobCards = WebDriverWait(driver, 10).until(
                    EC.presence_of_element_located((By.ID, "mosaic-provider-jobcards"))
                )
                logger.info("Mosaic of jobs has been found")

                # get joblink for each jobCard
                logger.info("Finding each job link")
                elements = jobCards.find_elements_by_class_name("tapItem")
                elements_count = len(elements)
                logger.info(f"Number of jobs found on this page : {elements_count}")
                for x in elements:
                    link = x.get_attribute("href")
                    job_links_list.append(link)
            finally:  # continue to next page
                pass

        job_link_count = len(job_links_list)
        logger.info(f"Number of total job links found : {job_link_count}")
        return job_links_list

    def format(self, job_link: str) -> Dict[str, Any]:
        """format [generates a dictionary of a job attributes, for each job link the function scraps with selenium and parse useful attributes.]

        Args:
            job_link (str): [job_link parsed after the function pull is executed. each job link is retrieved from the job_links_list scrapped in the pull function]

        Returns:
            Dict[str, Any]: [a job in the HrFlow job object format]
        """
        job = dict()

        logger.debug("Extracting job from a web page")
        driver = self.Crawler
        logger.debug(f"Crawler go to `{job_link}`")
        driver.get(job_link)
        logger.debug(f"Scraping the web page...")

        # name
        job["name"] = driver.find_element_by_class_name(
            "jobsearch-JobInfoHeader-title"
        ).text

        # reference
        m = re.search("jk=(.+?)&", job_link)
        if m:
            job["reference"] = m.group(1)
        else:
            job["reference"] = None

        # created_at, updated_at : isn't shown on ideed, TODO : convert "il y a n jours" into date time
        job["created_at"] = None
        job["updated_at"] = None

        # location
        # Sometimes there is a `COMPANY\nREVIEWS\nLOCATION'
        # Sometimes there is a `COMPANY\nLOCATION'
        subtitle_list = driver.find_element_by_class_name(
            "jobsearch-DesktopStickyContainer-subtitle"
        ).text.split("\n")

        if len(subtitle_list) >= 2:
            location = subtitle_list[-1]
        else:
            # In the case where there is only one item
            # It is assumed that the single element in the list is not a location
            location = "Undefined"

        job["location"] = dict(text=location, lat=None, lng=None)

        # url
        job["url"] = job_link

        # summary
        try:
            text = driver.find_element_by_id("jobDescriptionText").text
        except NoSuchElementException:
            text = None

        job["summary"] = text

        # description
        job["sections"] = [
            {"name": "description", "title": "description", "description": text}
        ]
        # compensation and jobType if they exist are in the same header so we get them both in one task of finding elements
        try:
            element = driver.find_element_by_class_name(
                "jobsearch-JobMetadataHeader-item"
            ).text
        except NoSuchElementException:
            element = "Null"

        # compensation
        # Need to be sure that we are parsing a salary and not a job Type because of indeed dynamic structure
        items = element.split()
        l = [
            "Temps plein",
            "Temps",
            "plein",
            "CDI",
            "plein,",
            "Stage",
            "CDD",
            "Apprentissage",
            "Alternance",
            "partiel",
        ]
        for item in l:
            if item in items:
                items.remove(item)
        salary = " ".join([items[i] for i in range(len(items))])

        # employment_type
        # avoid that Selenium parse useless and erronous information due to Indeed dynamic website architecture for example it can give jobType = "38 000 € par an - CDI" or "2 369 € - 4 645 € par mois - Temps plein, CDD"
        contract_type = ""
        if element not in [
            "CDI",
            "CDD",
            "Stage",
            "Alternance",
            "Temps plein",
            "Temps partiel",
            "Apprentissage",
            "Contrat Pro",
            "Stage, alternance",
        ]:
            if "CDI" in element.split():
                contract_type = "CDI"
            elif "plein" in element.split():
                contract_type = "CDI"
            elif "CDD" in element.split():
                contract_type = "CDD"
            else:
                contract_type = "null"
        jobType = contract_type

        job["tags"] = [
            dict(name="indeed_compensantion", value=salary),
            dict(name="indeed_employment_type", value=jobType),
        ]

        job["ranges_date"] = list()
        job["ranges_float"] = list()
        job["metadatas"] = list()

        driver.quit()
        logger.debug(f"The web page has been scraped")
        return job
