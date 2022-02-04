<p align="center">
  <a href="https://hrflow.ai">
    <img alt="hrflow" src="https://img.riminder.net/logo-hrflow.svg" width="120" />
  </a>
</p>
<h1 align="center">
  HrFlow.ai connectors
</h1>

![GitHub Repo stars](https://img.shields.io/github/stars/Riminder/hrflow-connectors?style=social) ![](https://img.shields.io/github/v/release/Riminder/hrflow-connectors) ![](https://img.shields.io/github/license/Riminder/hrflow-connectors)


<p align="center">
  <a href="https://hrflow.ai">
    <img alt="hrflow" src="https://hrflow-ai.imgix.net/corporate.svg"/>
  </a>
</p>

<br/>

## About HrFlow.ai

**[HrFlow.ai](https://hrflow.ai/) is on a mission to make AI and data integration pipelines a commodity in the HR Industry:**
  1. **Unify**: Link your Talent Data channels with a few clicks, so they can share data.
  2. **Understand**: Leverage our AI solutions to process your Talent Data.
  3. **Automate**: Sync data between your tools and build workflows that meet your business logic.

  `hrflow-connectors` is an open-source project created by **HrFlow.ai** 
to democratize Talent Data integration within the HR Tech landscape.

## :electric_plug: List of Connectors

| Name | Type | Available | Release date | Update date
| - | - | - | - | - |
| **ADP** |  | :hourglass: |  |  |
| [**Bullhorn**](src/hrflow_connectors/connectors/bullhorn/) | ATS | :heavy_check_mark: | *24/01/2022*  |*31/01/2022*   |
| [**Breezy.hr**](src/hrflow_connectors/connectors/breezyhr) | ATS | :heavy_check_mark: | *18/01/2022*  |*31/01/2022*   |
| **Cegid (Meta4)** |  | :hourglass: |  |  |
| [**Ceridian**](src/hrflow_connectors/connectors/ceridian) | HCM |:heavy_check_mark: | *12/01/2022*   |*31/01/2022*   |
| **Cornerstone OnDemand** |  | :hourglass: |  |  |
| [**Crosstalent**](src/hrflow_connectors/connectors/crosstalent) | ATS | :heavy_check_mark: | *21/12/2021*  |*31/01/2022*   |
| **Digitalrecruiters** | ATS | :hourglass: |  |  |
| **Fieldglass SAP** | Recruiting software | :hourglass: |  |  |
| [**Flatchr**](src/hrflow_connectors/connectors/flatchr/) | ATS | :heavy_check_mark: | *29/12/2021*  |*31/01/2022*   |
| [**Greenhouse**](src/hrflow_connectors/connectors/greenhouse) | ATS | :heavy_check_mark: |*04/01/2022*  | *31/01/2022*  |
| **ICIMS** |  | :hourglass: |  |  |
| **Jobvite** |  | :hourglass: |  |  |
| **Kronos (UKG)** |  | :hourglass: |  |  |
| **Laponi** | Job board | :hourglass: |  |  |
| **Lever** |  | :hourglass:  |  |  |
| **Mailchimp** | Marketing tools | :hourglass: |  |  |
| **Microsoft Dynamics** | HCM CLoud | :hourglass: |  |  |
| [**Monster**](src/hrflow_connectors/connectors/monster/) | Job board | :heavy_check_mark: | *06/01/2022* |*31/01/2022*   |
| **Oracle** | Cloud Apps | :hourglass: |  |  |
| [**Recruitee**](src/hrflow_connectors/connectors/recruitee/) | ATS | :heavy_check_mark: | *12/01/2022*  |*31/01/2022*   |
| **RecruitBox** |  | :hourglass: |  |  |
| [**SAPSuccessfactors**](src/hrflow_connectors/connectors/sapsuccessfactors/) | Cloud Apps for HR | :heavy_check_mark: | *07/01/2022* | *31/01/2022*  |
| **Salesforce** |  | :hourglass: |  |  |
| [**Smartrecruiters**](src/hrflow_connectors/connectors/smartrecruiters/) | ATS | :heavy_check_mark: | *29/12/2021*  |*31/01/2022*   |
| **Staffme** | Job board | :hourglass: |  |  |
| [**Taleez**](src/hrflow_connectors/connectors/taleez/)| ATS | :heavy_check_mark: |*12/01/2022*  |*31/01/2022*   |
| **Talentsoft** |  | :hourglass: |  |  |
| **Talentlink** |  | :hourglass: |  |  |
| [**Teamtailor**](src/hrflow_connectors/connectors/teamtailor) | ATS | :heavy_check_mark: |*24/01/2022*   |*31/01/2022*   |
| **Twilio** | Marketing tools | :hourglass: |  |  |
| **Ultimate Software (UKG)** |  | :hourglass: |  |  |
| [**Workable**](src/hrflow_connectors/connectors/workable/) | ATS | :heavy_check_mark: | *07/01/2022*  |*31/01/2022*   |
| **Workday** |  | :hourglass: |  |  |
| [**XML**](src/hrflow_connectors/connectors/xml/) | Job board | :heavy_check_mark: | *29/12/2021*  | *31/01/2022*  |

## 🪄 Quickstart
### What I can do?
With Hrflow Connector, you can **synchronize** and **process** multiple **HR data streams** in just a few lines of code.

You can do 4 types of actions:
* Pull jobs : `Job flow` :arrow_right: ***`Hrflow.ai Board`***
* Pull profiles : `Profile flow` :arrow_right: ***`Hrflow.ai Source`***
* Push job : ***`Hrflow.ai Board`*** :arrow_right: `External destination`
* Push profile : ***`Hrflow.ai Source`*** :arrow_right: `External destination`

The features offered by this package:
* **Synchronize an entire data** stream with a ready-to-use solution
*  **Synchronize only certain data** in a stream meeting a condition defined by you : [`logics`](DOCUMENTATION.md#logics)
* **Format the data as you wish** or use the default formatting that we propose adapted to each connector : [`format` & `format_function_name`](DOCUMENTATION.md#format)
* **Enrich the data processed with *Hrflow.ai's Parsing*** by activating only one option in the connector : [`hydrate_with_parsing=True`](DOCUMENTATION.md#using-parsing-to-enrich-a-job)

### How to use a connector ?
**Prerequisites**
* [✨ Create a Workspace](https://hrflow.ai/signup/)
* [🔑 Get your API Key](https://developers.hrflow.ai/docs/api-authentification)

1. **`pip install hrflow-connectors`**
2. **Configure the connector**. Let's take for example Greenhouse [Pull jobs](src/hrflow_connectors/connectors/greenhouse).
```python
from hrflow_connectors import Greenhouse
from hrflow import Hrflow

client = Hrflow(api_secret="MY_X-API-KEY", api_user="MY_X-USER-EMAIL")
Greenhouse.pull_jobs(
    board_token="MY_GREENHOUSE_BOARD_TOKEN",
    hrflow_client=client,
    board_key="MY_HRFLOW_BOARD_KEY"
)
```

🐇 **TADA! You have just called your first connector.**


## 📖 Documentation
To find out **how to use the connectors in detail**, you can take a look at the [📖 documentation](DOCUMENTATION.md).

## 🚀 Environment
Hrflow connector was initially developed to be integrated into [***workflows***](https://developers.hrflow.ai/docs/workflows).
For this reason **it is compatible with Python 3.6+**.

The project mainly uses 3 packages :
* `requests=="2.26.0"`
* `hrflow=="1.9.0"`
* `pydantic=="1.7.4"`

**To find the list of dependencies, you can look at the [`pyproject.toml`](pyproject.toml) file**

## :woman_technologist: Contributions

Please feel free to contribute to the quality of this content by
submitting PRs for improvements to code, architecture, etc. 

Any contributions you make to this effort are of course greatly 
appreciated.

👉 **To find out more about how to proceed, the rules and conventions to follow, read carefully [`CONTRIBUTING.md`](CONTRIBUTING.md).**

## 🔗 Resources
* Our Developers documentation : https://developers.hrflow.ai/
* Our API list (Parsing, Revealing, Embedding, Searching, Scoring, Reasoning) : https://www.hrflow.ai/api
* Our cool demos labs : https://labs.hrflow.ai

## :page_with_curl: License

See the [`LICENSE`](LICENSE) file for licensing information.
