import os
import datetime
from selenium import webdriver 
from hrflow import Hrflow
import requests
import time


class Crawler:
    """
    selenium Crawler Class
    """
    def __init__(self):
        chrome_options = webdriver.ChromeOptions()
        self._tmp_folder = '/tmp/chromium'
        if not os.path.exists(self._tmp_folder):
            os.makedirs(self._tmp_folder)
        if not os.path.exists(self._tmp_folder + '/user-data'):
            os.makedirs(self._tmp_folder + '/user-data')
        if not os.path.exists(self._tmp_folder + '/data-path'):
            os.makedirs(self._tmp_folder + '/data-path')
        if not os.path.exists(self._tmp_folder + '/cache-dir'):
            os.makedirs(self._tmp_folder + '/cache-dir')
        if not os.path.exists(self._tmp_folder + '/download-data'):
            os.makedirs(self._tmp_folder + '/download-data')
        self.download_location = self._tmp_folder + '/download-data'

        chrome_options.add_argument('--headless')
        chrome_options.add_argument('--no-sandbox')
        chrome_options.add_argument('--disable-gpu')
        chrome_options.add_argument('--window-size=1280x1696')
        chrome_options.add_argument('--user-data-dir={}'.format(self._tmp_folder + '/user-data'))
        chrome_options.add_argument('--hide-scrollbars')
        chrome_options.add_argument('--enable-logging')
        chrome_options.add_argument('--log-level=0')
        chrome_options.add_argument('--v=99')
        chrome_options.add_argument('--single-process')
        chrome_options.add_argument('--data-path={}'.format(self._tmp_folder + '/data-path'))
        chrome_options.add_argument('--ignore-certificate-errors')
        chrome_options.add_argument('--homedir={}'.format(self._tmp_folder))
        chrome_options.add_argument('--disk-cache-dir={}'.format(self._tmp_folder + '/cache-dir'))
        chrome_options.add_argument('--disable-dev-shm-usage')
        chrome_options.binary_location = "/opt/bin/headless-chromium"
        self._driver = webdriver.Chrome(chrome_options=chrome_options)

        print("Headless Chrome Initialized")
        params = {'behavior': 'allow', 'downloadPath': self._tmp_folder + '/download-data'}
        self._driver.execute_cdp_cmd('Page.setDownloadBehavior', params)

    def get_driver(self):
        return self._driver


def format_job(driver: object) -> dict:
    """
    Format the scrapped job according to the HrFlow.ai Job format
    @param driver: Crawler driver
    @return: job in the HrFlow.ai format of skills
    """
    location = driver.find_element_by_xpath("//*[@id='map']")
    tags = driver.find_element_by_xpath("//*[@class='attrgroup']").text.split('\n')
    
    return {
        "name": driver.find_element_by_xpath("//*[@id='titletextonly']").text,
        "agent_key": None,
        "reference": None,
        "url": driver.current_url,
        "created_at": driver.find_elements_by_xpath("//*[@class='postinginfo reveal']")[0].find_element_by_tag_name('time').get_attribute("datetime"),
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
    skills = [{ "name": text[ent['start']:ent['end']], "value": None, "type": "hard" if ent['label'] == "HardSkill" else "soft"} for ent in ents if ent['label'] in ["HardSkill", "SoftSkill"]]
    return list(set(skills))


def workflow(settings: dict) -> None:
    """
    PULL WORKFLOW allows you to run the following code instructions on a regular basis
    @rtype: None
    @param settings: dictionnary of settings params of the workflow
    """
    print('HrFlow.ai client')
    hrflow_client = Hrflow(api_secret=settings["API_KEY"], api_user=settings["USER_EMAIL"])
    
    c = Crawler()
    driver = c.get_driver()
    driver.get(settings["JOBBOARD_URL"])
    driver.maximize_window()
    total_jobs = int(driver.find_element_by_xpath("//*[@class='totalcount']").text)
    jobs = driver.find_elements_by_xpath("//*[@class='result-heading']")
    for i in range(0, total_jobs):
        driver.get(jobs[i].find_element_by_tag_name('a').get_attribute("href"))
        reference = driver.find_element_by_xpath("//*[@class='postinginfo']").text.split(':')[0].strip()
        job_hrflow = hrflow_client.job.indexing.get(board_key=settings["BOARD_KEY"], reference=reference).get('data')

        if job_hrflow:
            driver.find_element_by_xpath("//*[@class='next']").click()
            pass
        else: 
            try:
                job = format_job(driver)
                job["reference"] = reference
                job["agent_key"] = settings['AGENT_KEY']
                # Parse skills
                SECTION_SEPARATOR = "\n\n"  # important to separate sections by double line jumps
                job_text = SECTION_SEPARATOR.join(section['description'] or "" for section in job["sections"])
                job_parsing = hrflow_client.document.parsing.post(text=job_text).get('data')
                job['skills'] = format_skills(job_text, job_parsing["ents"])
                print("Save job")
                hrflow_client.job.indexing.add_json(board_key=settings["BOARD_KEY"], job_json=job)
            except requests.exceptions.RequestException:
                print('Saving job with reference %s failed'%(reference))
        driver.get(settings['JOBBOARD_URL'])
        jobs = driver.find_elements_by_xpath("//*[@class='result-heading']")
