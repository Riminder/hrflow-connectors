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
from typing import  Any


settings = {
    'JOBBOARD_URL' : 'https://fr.indeed.com',
    'JOBBOARD_URL_PAGINATION' : 'https://fr.indeed.com/emplois?q={}&l={}&limit={}&start={}',
    'BOARD_KEY' : "write in your board key",
    'API_KEY' : 'write in your api_key',
    'AGENT_KEY' : '',
    'USER_EMAIL' : 'write you api user key',
    'JOB_SEARCH' : 'write in the job you want to search',
    'JOB_LOCATION' : 'write in your job location',
    'LIMIT' : 20,
    'LIMIT_EXTRACT' : 5
}


class Crawler:

    """
    Selenium Crawler Class

    """

    def __init__(self) -> object:


        chrome_options = webdriver.ChromeOptions()
        chrome_options.add_argument("--headless")
        chrome_options.add_argument('--no-sandbox')
        chrome_options.add_argument('--disable-dev-shm-usage')
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


class SkillType(str, Enum):
    SoftSkill = "soft"
    HardSkill = "hard"
    Language = "language"
    Other = "other"



    
class getAllJobs(Crawler):


    def __init__(self, settings):

        
        self._job_search = settings["JOB_SEARCH"]
        self._job_location = settings["JOB_LOCATION"]
        self._limit = settings["LIMIT"]
        self._user_mail = settings["USER_EMAIL"]
        self._jobboard_url = settings["JOBBOARD_URL"]
        self._jobboardurl_pagination = settings["JOBBOARD_URL_PAGINATION"]
        self._limit_extract = settings["LIMIT_EXTRACT"]
        self._api_key = settings["API_KEY"]
        self._board_key = settings["BOARD_KEY"]
        self._agent_key = settings["AGENT_KEY"]
        self._settings = settings

        

    

        

    def base_url(self, pagination: int) -> str:

        '''
        Params:
            job_search : Job to look for 
            job_location : Location
            limit : Number of job per page
            pagination :  Page number
            
        Desc :
            Generate a url.
        Returns :
            string url.
        '''


        url_template = self._jobboardurl_pagination
        url = url_template.format(self._job_search, self._job_location, self._limit, pagination)
        return url



 
    


    class SkillType(str, Enum):
        SoftSkill = "soft"
        HardSkill = "hard"
        Language = "language"
        Other = "other"



    def indeed_scrapper(self) -> tuple:

        """
        scrapping the important attributes of a job card(Indeed pages show at first jobs in cards format),
        to do that you should use selenium 
        """
        jobsTitles = []
        jobsLinks = []
        jobsSalaries =[]
        jobSummaries = []
        jobsLocations = []
        jobsRefs = []

        c = Crawler()
        driver = c.get_driver()
        driver.get(self._jobboard_url)
        #Find the text box and enter the type of job the user entered earlier
        search = driver.find_element_by_id("text-input-what")
        search.send_keys(self._job_search)
        search.send_keys(Keys.RETURN)

        total_jobs = 248
        count_jobs = self._limit

        total_page = int(total_jobs / count_jobs)


        for page in range(0, total_page): 
            if page == settings['LIMIT_EXTRACT']:
                break
            page_url = self.base_url( pagination =  (page * count_jobs))
            driver.get(page_url)
            sleep(10)



            #Try to locate the id mentioned below, if it is not found within 10 seconds then close the program
            try:
                jobCards = WebDriverWait(driver, 10).until(
                    EC.presence_of_element_located((By.ID, "mosaic-provider-jobcards"))
                )

            


                #Collects all of the job titles on the page
                elements = jobCards.find_elements_by_tag_name("span")
                for x in elements:
                    val = x.get_attribute("title")
                    #Make sure that the empty values aren't shown
                    if not len(val.strip()) == 0:
                        jobsTitles.append(val)

                elements = jobCards.find_elements_by_class_name("job-snippet")
                for x in elements:
                    val = x.text
                    
                    jobSummaries.append(val)

            


                elements = jobCards.find_elements_by_class_name("companyLocation")
                for x in elements:
                    val = x.text
                    jobsLocations.append(val)


                elements = jobCards.find_elements_by_class_name("salary-snippet")
                for x in elements:
                    val = x.text
                    jobsSalaries.append(val)

                elements = jobCards.find_elements_by_tag_name("a")
                for x in elements:
                    val = x.get_attribute("data-jk")
                    jobsRefs.append(val)


                elements = jobCards.find_elements_by_class_name("tapItem")
                for x in elements:
            
                    val = x.get_attribute("href")
                
                    jobsLinks.append(val)

            finally:
                pass


            #After everything has been done close the program
        return (jobsTitles, jobsLocations, jobsSalaries, jobsRefs, jobsLinks, jobSummaries)



    def get_descriptions(self, jobLink : str) -> str:
        c = Crawler()
        driver = c.get_driver()

        driver.get(jobLink)

        description = driver.find_element_by_id("jobDescriptionText").text

        return description


    def format_entities(self ,desc_parsing) -> dict:
        description = desc_parsing['text']
        skills = filter(lambda x: x["label"] in ["HardSkill", "SoftSkill", "Language"], desc_parsing['ents'])
        tasks = filter(lambda x: x["label"] == "Task", desc_parsing['ents'])
        certifications = filter(lambda x: x["label"] == "Certification", desc_parsing['ents'])
        courses = filter(lambda x: x["label"] == "Course", desc_parsing['ents'])
        def transform(obj):
            return dict(name=description[obj["start"]:obj["end"]], value=None)
        skills = list(map(lambda x: dict(**transform(x), type=SkillType[x["label"]].value), skills))
        tasks = list(map(transform, tasks))
        certifications = list(map(transform, certifications))
        courses = list(map(transform, courses))
        return dict(skills=skills, tasks=tasks, certifications=certifications, courses=courses)


    def to_hrflow_job_format(self):

        hrflow_client = hrflow_client = Hrflow(api_secret=self._api_key, api_user=self._user_mail)

        (jobsTitles, jobsLocations, jobsSalaries, jobsRefs, jobsLinks, jobSummaries) = self.indeed_scrapper()

        for i in range(len(jobsTitles)):
            try:
                job_json={
                    "name": jobsTitles[i],
                    "agent_key": None,
                    "reference": jobsRefs[i],
                    "url": jobsLinks[i],
                    "created_at": None,
                    "updated_at": None,
                    "summary": jobSummaries[i],
                    "location": {
                        "text": jobsLocations[i],
                        "lat": None,
                        "lng": None},
                    "sections": [
                        {"name": "description", "title": "Description", "description": self.get_descriptions(jobsLinks[i])}
                    ],
                    "skills": [],
                    "languages": [],
                    "tags": [
                        {"name": "compensation", "value": jobsSalaries[i]},
                        {"name": "employment_type", "value": None}
                    ],
                    "ranges_date": [],
                    "ranges_float": [],
                    "metadatas": [],
                }
            
            except IndexError:

                job_json = {
                    "name": jobsTitles[i],
                    "agent_key": None,
                    "reference": jobsRefs[i],
                    "url": jobsLinks[i],
                    "created_at": None,
                    "updated_at": None,
                    "summary": jobSummaries[i],
                    "location": {
                        "text": jobsLocations[i],
                        "lat": None,
                        "lng": None},
                    "sections": [
                        {"name": "description", "title": "Description", "description": self.get_descriptions(jobsLinks[i])}
                    ],
                    "skills": [],
                    "languages": [],
                    "tags": [
                        {"name": "compensation", "value": None},
                        {"name": "employment_type", "value": None}
                    ],
                    "ranges_date": [],
                    "ranges_float": [],
                    "metadatas": [],
                }


            if len(self.get_descriptions(jobsLinks[i])) > 0: 

                SECTION_SEPARATOR = "\n\n"  # important to separate sections by double line jumps
                job_text = SECTION_SEPARATOR.join(section["description"] or "" for section in job_json["sections"])

                desc_parsing = hrflow_client.document.parsing.post(text = job_text)
                desc_parsing = desc_parsing["data"]
                formated = self.format_entities(desc_parsing)
                job_json.update(formated)


            hrflow_client.job.indexing.add_json(board_key="", job_json=job_json)




def workflow(settings):




    all = getAllJobs(settings)


    pull = all.to_hrflow_job_format()


    return pull  


workflow(settings)
