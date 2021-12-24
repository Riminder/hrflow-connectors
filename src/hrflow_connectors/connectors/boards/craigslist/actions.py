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


        