<p align="center">
  <a href="https://hrflow.ai">
    <img alt="hrflow" src="https://img.riminder.net/logo-hrflow.svg" width="120" />
  </a>
</p>
<h1 align="center">
  HrFlow.ai connectors
</h1>

![](https://img.shields.io/github/v/release/Riminder/hrflow-connectors) ![](https://img.shields.io/github/license/Riminder/hrflow-connectors)


<p align="center">
  <a href="https://hrflow.ai">
    <img alt="hrflow" src="https://hrflow-ai.imgix.net/corporate.svg"/>
  </a>
</p>

<br/>

  `hrflow-connectors` is an open source project created by **HrFlow.ai** 
to allow developers to connect easily HR ecosystem component.

This project is basically designed to handle simply and easily,
complex HR integrations by using [**workflows**](https://developers.hrflow.ai/docs/workflows) feature.
<br/>

## About HrFlow.ai
  https://www.HrFlow.ai is **an API first company that provides the most sophisticated AI Powered JOB & PROFILE API**. Corporates and Software vendors can leverage our technology layers to Parse, Enrich and Score both job and candidate data. The platform supports +200 apps, pipelines and code integrations so you can automate workflows with your favorite tools.
  - Our Developers documentation : https://developers.hrflow.ai/
  - Our API list (Parsing, Revealing, Embedding, Searching, Scoring, Reasoning) : https://www.hrflow.ai/api
  - Our cool demos labs : https://labs.hrflow.ai


## 🪄 Quick start
### What I can do ?
With Hrflow Connector, you can **synchronize** and **process** multiple **HR data streams** in just a few lines of code.

You can do 3 types of actions:
* `Job flow` :arrow_right: ***`Board Hrflow.ai`***
* `Profile flow` :arrow_right: ***`Source Hrflow.ai`***
* ***`Hrflow.ai`*** :arrow_right: `External destination`

The features offered by this package:
* **Synchronise an entire data** stream with a ready-to-use solution
*  **Synchronise only certain data** in a stream meeting a condition defined by you : [`logics`]()
* **Format the data as you wish** or use the default formatting that we propose adapted to each connector : [`format` & `format_function_name`]()
* **Enrich the data processed with *Hrflow.ai's Parsing*** by activating only one option in the connector: [`hydrate_job_with_parsing=True`]()

### How to use a connector ?
**Prerequisites**
* [✨ Create a Workspace](https://hrflow.ai/signup/)
* [🔑 Get your API Key](https://developers.hrflow.ai/docs/api-authentification)

1. **`pip install hrflow-connectors`**
2. **I configure my connector** that I want to use. Let's take for example Crosstalent [`GetAllJobs`](src/hrflow_connectors/connectors/destinations/crosstalent).
```python
from hrflow import Hrflow

from hrflow_connectors.core.auth import OAuth2PasswordCredentialsBody
from hrflow_connectors.connectors.boards.crosstalent import GetAllJobs

# 2.1 Hrflow client configuration
client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")

# 2.2 Configuring authentication to Crosstalent via the Salesforce API
access_token_url = "https://test.salesforce.com/services/oauth2/token"
auth = OAuth2PasswordCredentialsBody(
    access_token_url=access_token_url,
    client_id="MY_CLIENT_ID",
    client_secret="MY_CLIENT_SECRET",
    username="MY_USERNAME",
    password="MY_PASSWORD",
)

# 2.3 General connector configuration
action = GetAllJobs(
    auth=auth,
    subdomain="MY_SUBDOMAIN",
    hrflow_client=client,
    board_key="MY_BOARD_KEY",
    hydrate_with_parsing=True,
)
```
3.  I write the line of code `action.execute()` to **run the connector.**

🐇 **TADA ! You have just called your first connector.**


## 📖 Documentation
To find out **how to use the connectors in detail**, you can take a look at the [📖 documentation](DOCUMENTATION.md).

## 🚀 Environment
Hrflow connector was initially developed to be integrated into [***workflows***](https://developers.hrflow.ai/docs/workflows).
For this reason **it is compatible with Python 3.6+**.

The project mainly uses 4 packages :
* `requests=="2.26.0"`
* `hrflow=="1.9.0"`
* `pydantic=="1.7.4"`
* `selenium=="3.141.0"`

**To find the list of dependencies, you can look at the [`pyproject.toml`](pyproject.toml) file**

## :electric_plug: List of Connectors
- **ADP** (soon)
- **Craigslist** : [`CraigslistFeed`](src/hrflow_connectors/connectors/boards/craigslist/)
- **Careerbuilder** (soon)
- **Cegid(Meta4)** (soon)
- **Ceridian** (soon)
- **Cornerstone OnDemand** (soon)
- **Crosstalent** : [`GetAllJobs`](src/hrflow_connectors/connectors/boards/crosstalent), [`PushProfile`](src/hrflow_connectors/connectors/destinations/crosstalent)
- **Digitalrecruiters** (soon)
- **Flatchr** : [`PushProfile`](src/hrflow_connectors/connectors/destinations/flatchr/), [`EnrichProfile`](src/hrflow_connectors/connectors/destinations/flatchr/)
- **Indeed** : [`IndeedFeed`](src/hrflow_connectors/connectors/boards/indeed)
- **Kronos(UKG)** (soon)
- **Laponi** (soon)
- **Mailchimp** (soon)
- **Monster** (soon)
- **Oracle** (soon)
- **SAP Successfactors** (soon)
- **Salesforce** (soon)
- **Smartrecruiters** : [`SmartJobs`](src/hrflow_connectors/connectors/boards/smartrecruiters/)
- **Staffme** (soon)
- **Talentsoft** (soon)
- **Twilio** (soon)
- **Ultimate Software(UKG)** (soon)
- **Workday** (soon)

## 🧐 What"s inside?
TODO
A quick look at the top-level files of a **connector code structure**.

* **`legacy/`** :
* **`src/hrflow_connectors/connectors/`** : 
  * **`boards/`** : 
  * **`sources/`** :
  * **`destinations/`** :

* **`tests/`** :
* **`.coveragerc`** :
* **`.gitignore`** : This file tells git which files it should not track / not maintain a version history for.
* **`AUTHORS.md`** :
* **`CHANGELOG.md`** :
* **`CONTRIBUTING.md`** :
* **`LICENSE`** :
* **`README.md`**: A text file containing useful reference information about this project.
* **`LICENSE`** :
* **`VERSION`** :
* **`poetry.lock`** :
* **`pyproject.toml`** :
* **`pytest.ini`** :
* **`setup.cfg`** :
* **`setup.py`** :

## :woman_technologist: Contributions

TODO: Rediriger vers CONTRIBUTING

Please feel free to contribute to the quality of this content by
submitting PRs for improvements to code, architecture, etc. 
While typo fixes are welcomed, they will likely be caught through 
normal editing/publishing processes, so please don"t worry about 
them right now.

Any contributions you make to this effort are of course greatly 
appreciated.


## :page_with_curl: License

See the [`LICENSE`](LICENSE) file for licensing information.