import os
from selenium import webdriver
from hrflow import Hrflow
import requests


class Crawler:
    """
    selenium Crawler Class
    """
    def __init__(self) -> object:
        chrome_options = webdriver.ChromeOptions()
        self._tmp_folder = "/tmp/chromium"
        if not os.path.exists(self._tmp_folder):
            os.makedirs(self._tmp_folder)
        if not os.path.exists(self._tmp_folder + "/user-data"):
            os.makedirs(self._tmp_folder + "/user-data")
        if not os.path.exists(self._tmp_folder + "/data-path"):
            os.makedirs(self._tmp_folder + "/data-path")
        if not os.path.exists(self._tmp_folder + "/cache-dir"):
            os.makedirs(self._tmp_folder + "/cache-dir")
        if not os.path.exists(self._tmp_folder + "/download-data"):
            os.makedirs(self._tmp_folder + "/download-data")
        self.download_location = self._tmp_folder + "/download-data"

        chrome_options.add_argument("--headless")
        chrome_options.add_argument("--no-sandbox")
        chrome_options.add_argument("--disable-gpu")
        chrome_options.add_argument("--window-size=1280x1696")
        chrome_options.add_argument("--user-data-dir={}".format(self._tmp_folder + "/user-data"))
        chrome_options.add_argument("--hide-scrollbars")
        chrome_options.add_argument("--enable-logging")
        chrome_options.add_argument("--log-level=0")
        chrome_options.add_argument("--v=99")
        chrome_options.add_argument("--single-process")
        chrome_options.add_argument("--data-path={}".format(self._tmp_folder + "/data-path"))
        chrome_options.add_argument("--ignore-certificate-errors")
        chrome_options.add_argument("--homedir={}".format(self._tmp_folder))
        chrome_options.add_argument("--disk-cache-dir={}".format(self._tmp_folder + "/cache-dir"))
        chrome_options.add_argument("--disable-dev-shm-usage")
        chrome_options.binary_location = "/opt/bin/headless-chromium"
        self._driver = webdriver.Chrome(chrome_options=chrome_options)

        print("Headless Chrome Initialized")
        params = {"behavior": "allow", "downloadPath": self._tmp_folder + "/download-data"}
        self._driver.execute_cdp_cmd("Page.setDownloadBehavior", params)

    def get_driver(self):
        return self._driver


def format_job(driver: object) -> dict:
    """
    Format the scrapped job according to the HrFlow.ai Job format
    @param driver: Crawler driver
    @return: job in the HrFlow.ai format of skills
    """
    location = driver.find_element_by_xpath("//*[@id='map']")
    tags = driver.find_element_by_xpath("//*[@class='attrgroup']").text.split("\n")
    
    return {
        "name": driver.find_element_by_xpath("//*[@id='titletextonly']").text,
        "agent_key": None,
        "reference": None,
        "url": driver.current_url,
        "created_at": driver.find_elements_by_xpath("//*[@class='postinginfo reveal']")[0].find_element_by_tag_name("time").get_attribute("datetime"),
        "updated_at": None,
        "summary": "",
        "location": {
            "text": None,
            "lat": location.get_attribute("data-latitude"),
            "lng": location.get_attribute("data-longitude")},
        "sections": [
            {"name": "description", "title": "Description", "description": driver.find_element_by_xpath("//*[@id='postingbody']").text}
        ],
        "skills": [],
        "languages": [],
        "tags": [
            {"name": "compensation", "value": tags[0].split(":")[1].strip()},
            {"name": "employment_type", "value": tags[1].split(":")[1].strip()}
        ],
        "ranges_date": [],
        "ranges_float": [],
        "metadatas": [],
    }


def format_skills(text: str, ents: list) -> list:
    """
    Get the list of skills according to the HrFlow.ai Job format
    @param text: text description of the job
    @param ents: list of entities in the text
    @return: list of skills
    """
    skills = [{ "name": text[ent["start"]:ent["end"]].lower(), "value": None,
                "type": "hard" if ent["label"] == "HardSkill" else "soft"}
              for ent in ents if ent["label"].endswith("Skill")]
    return list({v["name"]:v for v in skills}.values())


def workflow(settings: dict) -> None:
    """
    PULL WORKFLOW allows you to run the following code instructions on a regular basis

    WORKFLOW follows these steps:
        1- Init HrFlow.ai Client
        2- Open Jobboard URL
        3- Compute total_pages
        4- Iterate over all pages
             - Iterate over all jobs in the given page
                - For every job, check whether the job already indexed in HrFlow.ai"s board using Job API
                - If the job doesn"t exist :
                        - scrap and format the job
                        - enrich the job using HrFlow.ai Document API
                        - save the job using HrFlow.ai Job API

    @rtype: null
    @param settings: dictionary of settings params of the workflow
    """
    hrflow_client = Hrflow(api_secret=settings["API_KEY"], api_user=settings["USER_EMAIL"])
    c = Crawler()
    driver = c.get_driver()
    # Get MAX PAGES
    driver.get(settings["JOBBOARD_URL"])
    driver.maximize_window()
    total_jobs = int(driver.find_element_by_xpath("//*[@class='totalcount']").text)
    count_jobs = 120 # count jobs per Page
    total_pages = total_jobs // count_jobs + 1
    for page in range(0, total_pages):
        page_url = settings["JOBBOARD_URL"]+"s=%s"%((page+1)*count_jobs)
        driver.get(page_url)
        jobs = driver.find_elements_by_xpath("//*[@class='result-heading']")
        total_jobs = int(driver.find_element_by_xpath("//*[@class='totalcount']").text)
        for i in range(0, total_jobs):
            driver.get(jobs[i].find_element_by_tag_name("a").get_attribute("href"))
            reference = driver.find_element_by_xpath("//*[@class='postinginfo']").text.split(":")[0].strip()
            verify_job = hrflow_client.job.indexing.get(board_key=settings["BOARD_KEY"], reference=reference).get("data")
            if not verify_job:
                try:
                    job = format_job(driver)
                    job["reference"] = reference
                    job["agent_key"] = settings["AGENT_KEY"]
                    # Parse skills
                    SECTION_SEPARATOR = "\n\n"  # important to separate sections by double line jumps
                    job_text = SECTION_SEPARATOR.join(section["description"] or "" for section in job["sections"])
                    job_parsing = hrflow_client.document.parsing.post(text=job_text).get("data")
                    job["skills"] = format_skills(job_text, job_parsing["ents"])
                    # Save Job
                    hrflow_client.job.indexing.add_json(board_key=settings["BOARD_KEY"], job_json=job)
                except requests.exceptions.RequestException:
                    raise Exception("Saving job with reference %s failed"%(reference))
