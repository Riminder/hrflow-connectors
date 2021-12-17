from typing import Dict, Any
from ....core.action import BoardAction
import re
from pydantic import Field
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from time import sleep
from selenium import webdriver
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
        #chrome_options.binary_location = "/opt/bin/headless-chromium" #use this in HrFlow workflows
        self._driver = webdriver.Chrome(executable_path= "chromedriver.exe" ,chrome_options=chrome_options) #use this for local running with the executable path as the Chromedriver path in your machine

    def get_driver(self):

        return self._driver


class GetAllJobs(BoardAction, Crawler):

    subdomain: str = Field(
        ...,
        description="Subdomain just before 'indeed.com' for example subdomain ='fr' in 'https:/fr.indeed.com ",
    )
    job_search: str = Field(
        ...,
        description="Name of the job position we want to search offers in 'fr.indeed.com'",
    )
    job_location: str = Field(..., description="Location of the job offers")

    limit: int = Field(
        15,
        description=" limit of jobs extracted on page, usually on 'indeed.com', the number of offers per page is 15",
    )
    limit_extract: int = Field(1, description=" limit of pages you want to extract")
    

    @property
    def url_base(self) -> str:
        return "https:/{}.indeed.com/".format(self.subdomain)


    def path(self, pagination:int) -> str:

        return "emplois?q={query}&l={location}&limit={limit}&start={start}".format(query = self.job_search, location = self.job_location, limit = self.limit, start = pagination)

    def pull(self) -> list:
        """the role of this function is to interact with indeed, click buttons and search offers based on job title and location.
        for each page we scrap all the job cards shown (usually 15 per page), and for each job card it retrieves its individual link
        """
        jobs_Links = []
        driver = Crawler().get_driver()
        driver.get(self.url_base)
        # Find the text box and enter the type of job the user wants to get offers data
        search = driver.find_element_by_id("text-input-what")
        search.send_keys(self.job_search)
        search.send_keys(Keys.RETURN)

        
            # Get total number of related job offers in all pages.
        total_job_s = driver.find_element_by_id('searchCountPages' ).text
        

        # retrieve the number of total related job offers from string 'for example from Page 1 de 993 emplois we get total_jobs = 993'
        total_job_s = total_job_s.split()
        start = total_job_s.index("de")
        end = total_job_s.index("emplois")
        total_jobs = int("".join([total_job_s[i] for i in range(start + 1, end)]))

        count_jobs = self.limit  # max 15 jobs per page
        total_page = int(total_jobs / count_jobs)

        for page in range(0, total_page):
            if page == self.limit_extract:  # break if page reaches limit set by user
                break
            page_url = self.url_base + self.path(pagination = count_jobs * page)
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
                    jobs_Links.append(link)

            finally:

                pass

        return jobs_Links

    def format(self, job_link: str) -> Dict[str, Any]:
        """
        parameter: job_link parsed after the function pull is executed. each job link is retrieved from the jobs_Links list scrapped in the pull function

        Description: generates a dictionary of a job attributes, for each job link the function scraps with selenium and parse useful attributes.

        returns: a job in the HrFlow job object format
        """
        job = dict()
        driver = Crawler().get_driver()
        driver.get(job_link)

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
        m = re.search("jk=(.+?)&tk", job_link)
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
            salary = 'Null'

        if salary:

            words = ['stage', 'CDI', 'Temps plein', 'CDD', 'Alternance'] 
            for w in words: # Need to be sure that we are parsing a salary and not a job Type because of indeed dynamic structure
                if w in salary:
                    salary = 'Null'
            


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
