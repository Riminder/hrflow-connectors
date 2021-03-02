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
    <img alt="hrflow" src="https://hrflow.ai/new/img/home/corporate.svg"/>
  </a>
</p>

<br/>
  hrflow-connectors is an open source project created by HrFlow.ai 
to allow developers to connect easily HR ecosystem component.

This project is basically designed to handle simply and easily,
complex HR integrations by using [**workflows**](https://developers.hrflow.ai/products-1/workflows) feature.
<br/>


## About HrFlow.ai
  https://www.HrFlow.ai is an API first company that provides the most sophisticated AI Powered JOB & PROFILE API. Corporates and Software vendors can leverage our technology layers to Parse, Enrich and Score both job and candidate data. The platform supports +200 apps, pipelines and code integrations so you can automate workflows with your favorite tools.
  - Our Developers documentation : https://developers.hrflow.ai/
  - Our API list (Parsing, Revealing, Embedding, Searching, Scoring, Reasoning) : https://www.hrflow.ai/api
  - Our cool demos labs : https://labs.hrflow.ai

## List of Connectors
- **ADP**(soon)
- [**Craigslist**](https://github.com/Riminder/hrflow-connectors/tree/master/Craigslist)
- **Careerbuilder**(soon)
- **Cegid(Meta4)**(soon)
- **Ceridian**(soon)
- **Cornerstone OnDemand**(soon)
- [**Crosstalent**](https://github.com/Riminder/hrflow-connectors/tree/master/Crosstalent)
- [**Digitalrecruiters**](https://github.com/Riminder/hrflow-connectors/tree/master/Digitalrecruiters)
- **Indeed**(soon)
- **Kronos(UKG)**(soon)
- [**Laponi**](https://github.com/Riminder/hrflow-connectors/tree/master/Laponi)
- [**Mailchimp**](https://github.com/Riminder/hrflow-connectors/tree/master/Mailchimp)
- **Monster**(soon)
- **Oracle**(soon)
- **SAP Successfactors**(soon)
- **Salesforce**(soon)
- [**Smartrecruiters**](https://github.com/Riminder/hrflow-connectors/tree/master/Smartrecruiters)
- [**Staffme**](https://github.com/Riminder/hrflow-connectors/tree/master/Staffme)
- [**Talentsoft**]((https://github.com/Riminder/hrflow-connectors/tree/master/Talentsoft))
- [**Twilio**](https://github.com/Riminder/hrflow-connectors/tree/master/Twilio)
- **Ultimate Software(UKG)**(soon)
- **Workday**(soon)
    

## 🚀 Environment
### 
Workflows runs in the following environment:

    python== 3.6
    hrflow==1.8.6
    requests==2.24.0
    selenium==3.141.0
    twilio==6.46.0
    scipy==1.5.1
    numpy==1.19.0

## 🧐 What"s inside?

  A quick look at the top-level files of a [**connector code structure**](https://github.com/Riminder/hrflow-connectors/tree/master/.ExampleConnector):


    .
    ├── Connector_name
    │   ├── profile.json 
    │   ├── job.json
    │   ├── pull_jobs_from_hrflow_into_connector.py 
    │   ├── pull_jobs_from_connector_into_hrflow.py
    │   ├── pull_profiles_from_hrflow_into_connector.py 
    │   ├── pull_profiles_from_connector_into_hrflow.py
    │   ├── push_job_from_hrflow_into_connector.py 
    │   ├── push_job_from_connector_into_hrflow.py
    │   ├── push_profiles_from_hrflow_into_connector.py 
    │   ├── push_profiles_from_connector_into_hrflow.py    
    │   ├── README.md
    ├── .gitignore
    ├── tutorial
    ├── LICENSE
    └── README.md

  1.  **`/connector_name`**: This directory will contain
      all of the code related to either **PULL** or **CATCH**
      profiles / jobs from HrFlow.ai into Connector and vice versa.

  2.  **`.gitignore`**: This file tells git which files it should not track / not maintain a version history for.

  3.  **`tutorial`**: Pull/Push workflow examples.
      
  4.  **`LICENSE`**: Apache License.

  5. **`README.md`**: A text file containing useful reference information about this project.

## :octocat: Contributions

  Please feel free to contribute to the quality of this content by
  submitting PRs for improvements to code, architecture, etc. 
  While typo fixes are welcomed, they will likely be caught through 
  normal editing/publishing processes, so please don"t worry about 
  them right now.

  Any contributions you make to this effort are of course greatly 
  appreciated.
