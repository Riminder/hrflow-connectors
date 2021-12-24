from typing import Dict, Any, Iterator, Optional
from ....core.action import BoardAction
from pydantic import Field
from selenium import webdriver

class CraigslistJobs(BoardAction):

    subdomain: str = Field(
        ...,
        description="Subdomain just before 'craigslist.org/d/emploi/search/jjj' for example subdomain =`paris` in `https://paris.craigslist.org/d/emploi/search/jjj`, it is also the localisation of the job offers ",
    )
    executable_path: Optional[str] = Field(
        None,
        description="A separate executable that Selenium WebDriver used to control Chrome. Make sure you install the chromedriver with the same version as your local Chrome navigator",
    )

    binary_location: Optional[str] = Field(
        None,
        description="Location of the binary chromium, usually in HrFlow workflows it equals `/opt/bin/headless-chromium`",
    )

    @property
    def base_url(self):

        return "https://{}.craigslist.org/d/emploi/search/jjj".format(self.subdomain)

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
        `pull`[The role of this function is to scrap and retrieve each job offer link for each job posted]

        Returns:
            Iterator[str]: [list of scrapped jobLinks]
        """
        job_link_list = list()
        driver = self.Crawler
        driver.get(self.base_url)
        driver.maximize_window()
        total_jobs = int(driver.find_element_by_xpath("//*[@class='totalcount']").text)
        count_jobs = 120 # count jobs per Page
        total_pages = total_jobs // count_jobs + 1
        for page in range(0, total_pages):
            driver.get(self.base_url + "s=%s"%((page+1)*count_jobs))
            jobs = driver.find_elements_by_xpath("//*[@id='search-results']/li")
            job_link_list += [job.find_element_by_tag_name("a").get_attribute("href") for job in jobs]


        return job_link_list

    def format(self, job_link:str) -> Dict[str, Any]:
        """
        `format`[generates a dictionary of a job attributes, for each job link the function scraps with selenium and parse useful attributes.]

        Args:
            job_link (str): [job_link parsed after the function pull is executed. each job link is retrieved from the job_links_list scrapped in the pull function]

        Returns:
            Dict[str, Any]: [a job in the HrFlow job object format]
        """
        job = dict()
        driver = self.Crawler
        driver.get(job_link)

        job["name"] = driver.find_element_by_xpath("//*[@id='titletextonly']").text
        job["reference"] = driver.find_element_by_xpath("//*[@class='postinginfo']").text.split(":")[0].strip()
        job["url"] = job_link
        job["created_at"] = driver.find_elements_by_xpath("//*[@class='postinginfo reveal']")[0].find_element_by_tag_name("time").get_attribute("datetime")
        job["updated_at"] = None
        job["summary"] = ""
        job["location"] = dict(text = self.subdomain, lat = None, lng = None)
        job["sections"] = [dict(name = "description", title = "Description", description = driver.find_element_by_xpath("//*[@id='postingbody']").text )]
        tags = driver.find_element_by_xpath("//*[@class='attrgroup']").text.split("\n")
        job["tags"] = [ dict(name = "compensatipn", value = tags[0].split(":")[1].strip()), dict(name = "employment_type", value = tags[1].split(":")[1].strip())]
        job["ranges_date"] = []
        job["ranges_float"] = []
        job["metadatas"] = []

        return job