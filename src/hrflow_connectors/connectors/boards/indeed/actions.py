from typing import Dict, Any, Iterator, Optional
from ....core.action import BoardAction
import re
from pydantic import Field
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from time import sleep

from selenium.common.exceptions import NoSuchElementException


class GetAllJobs(BoardAction):

    subdomain: str = Field(
        ...,
        description="Subdomain just before 'indeed.com' for example subdomain ='fr' in 'https:/fr.indeed.com ",
    )
    job_search: str = Field(
        ...,
        description="Name of the job position we want to search offers in 'fr.indeed.com'",
    )
    job_location: str = Field(..., description="Location of the job offers")

    executable_path: str = Field(
        ...,
        description="A separate executable that Selenium WebDriver uses to control Chrome. Make sure you install the chromedriver with the same version as your local Chrome navigator",
    )

    @property
    def url_base(self) -> str:
        return "https:/{}.indeed.com".format(self.subdomain)

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
        """chrome_options.binary_location = "/opt/bin/headless-chromium"""  # use this in HrFlow workflows
        """driver = webdriver.Chrome(chrome_options)"""  # use this in HrFlow workflows
        driver = webdriver.Chrome(
            executable_path=self.executable_path, chrome_options=chrome_options
        )  # use this for local running with the executable path as the Chromedriver path in your machine

        return driver

    def path(self, pagination: int) -> str:
        """path [generates the path and pagination of the job offers search]

        Args:
            pagination (int): for example pagination = 10 if we want to start from page 2

        Returns:
            str: for example it returns /emplois?q=data%20scientist&l=paris&start=10 if we are looking for Data Scientist offers in Paris in page 2
        """

        return "/emplois?q={query}&l={location}&start={start}".format(
            query=self.job_search,
            location=self.job_location,
            start=pagination,
        )

    def pull(self) -> Iterator[str]:
        """pull [the role of this function is to interact with indeed, click buttons and search offers based on job title and location.
        for each page we scrap all the job cards shown (usually 15 per page), and for each job card it retrieves its individual link]

        Returns:
            Iterator[str]: [list of scrapped jobLinks]
        """
        job_links_list = []
        driver = self.Crawler
        driver.get(self.url_base)
        # Find the text box and enter the type of job the user wants to get offers data
        search = driver.find_element_by_id("text-input-what")
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
        )  # group different parts of the number together for example if it is 'Page 1 de 1993 emplois' we get 1993

        # If there is only one job offer result -corresponding to 'emploi'- or none a value error is raised, it is a rare case and the custom should make sure to type in a real job name.

        count_jobs = 15  # maximum jobs shown per page 15, important for pagination
        total_page = int(
            job_search_count / count_jobs
        )  # for example for a total of 993 and a count of 15 we loop over 66 pages

        for page in range(0, total_page):

            page_url = self.url_base + self.path(pagination=count_jobs * page)
            driver.get(page_url)
            sleep(3)

            try:  # get jobCards inside of a page
                jobCards = WebDriverWait(driver, 10).until(
                    EC.presence_of_element_located((By.ID, "mosaic-provider-jobcards"))
                )
                # get joblink for each jobCard
                elements = jobCards.find_elements_by_class_name("tapItem")
                for x in elements:

                    link = x.get_attribute("href")
                    job_links_list.append(link)

            except:
                raise Exception(
                    "Runtime Error: Selenium Webdriver could not find elements, verify that the page is not empty"
                )

        return job_links_list

    def format(self, job_link: str) -> Dict[str, Any]:
        """format [generates a dictionary of a job attributes, for each job link the function scraps with selenium and parse useful attributes.]

        Args:
            job_link (str): [job_link parsed after the function pull is executed. each job link is retrieved from the job_links_list scrapped in the pull function]

        Returns:
            Dict[str, Any]: [a job in the HrFlow job object format]
        """
        job = dict()
        driver = self.Crawler
        driver.get(job_link)

        # name
        job["name"] = driver.find_element_by_class_name('jobsearch-JobInfoHeader-title').text
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
        job["url"] = job_link

        # summary
        job["summary"] = driver.find_element_by_id("jobDescriptionText").text

        # description
        description = driver.find_element_by_id("jobDescriptionText").text

        job["sections"] = [
            {"name": "description", "title": "description", "description": description}
        ]

        # compensation
        try:
            salary = driver.find_element_by_xpath(
                "/html/body/div[1]/div/div[1]/div[3]/div/div/div[1]/div[1]/div[2]/div[2]/span[1]"
            ).text

        except NoSuchElementException:
            salary = "Null"

        if salary:

            words = ["stage", "CDI", "Temps plein", "CDD", "Alternance"]
            for (
                w
            ) in (
                words
            ):  # Need to be sure that we are parsing a salary and not a job Type because of indeed dynamic structure
                if w in salary:
                    salary = "Null"

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

            jobType = None

        if jobType not in [
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
            jobType = None  # avoid that Selenium parse useless and erronous information due to Indeed dynamic website architecture

        job["tags"] = [
            dict(name="indeed_compensantion", value=salary),
            dict(name="indeed_employment_type", value=jobType),
        ]

        job["ranges_date"] = list()
        job["ranges_float"] = list()
        job["metadatas"] = list()

        driver.quit()

        return job
