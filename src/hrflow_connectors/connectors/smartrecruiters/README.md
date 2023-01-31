# 📖 Summary
- [📖 Summary](#-summary)
- [💼 About SmartRecruiters](#-about-smartrecruiters)
  - [😍 Why is it a big deal for SmartRecruiters customers \& partners?](#-why-is-it-a-big-deal-for-smartrecruiters-customers--partners)
- [🔧 How does it work?](#-how-does-it-work)
  - [📊 Data integration capabilities:](#-data-integration-capabilities)
  - [🧠 Artificial Intelligence capabilities:](#-artificial-intelligence-capabilities)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 💼 About SmartRecruiters

SmartRecruiters’ Talent Acquisition Suite is used by organizations to make the best hires. It has complete functionality for recruitment marketing and collaborative hiring built on a modern cloud platform with an open marketplace for 3rd party recruitment services.

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214608440-2d838ca1-cdd9-49ca-ac3b-25d65f06994a.png width=90% height=100% >
</p>

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214608101-4a496152-8aec-4528-afd0-b0712f9c5010.png width=90% height=100% >
</p>

## 😍 Why is it a big deal for SmartRecruiters customers & partners?

This new connector will enable:
- ⚡ A Fastlane Talent & Workforce data integration for SmartRecruiters customers & partners
- 🤖 Cutting-edge AI-powered Talent Experiences & Recruiter Experiences for SmartRecruiters customers


# 🔧 How does it work?
## 📊 Data integration capabilities:
- ➡️ Send Profiles data from a Source of your choice to SmartRecruiters
- ⬅️ Send Jobs data from SmartRecruiters to a Destination of your choice


## 🧠 Artificial Intelligence capabilities:
- Extract, Structure, and Categorize Talent & Workforce data
- Search, Score, and Match Profiles & Jobs with our APIs and AI Widgets


# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- |  -------- |
| [**Pull jobs**](docs/pull_jobs.md) | Retrieves all jobs from SmartRecruiters and sends them to an HrFlow board | 
| [**Pull profiles**](docs/pull_profiles.md) | Retrieves profiles from SmartRecruiters and sends them to an Hrflow.ai Source |
| [**Push profiles**](docs/push_profiles.md) | Sends profiles from an HrFlow Source to SmartRecruiters as a candidate for a given job |

</p>

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214609749-f47466cc-efe9-4132-b81b-dc0dcfe77464.png width=90% height=100% >
</p>


# 🐍 Quick Start Examples

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**. To do this, execute the following steps in a new virtual environment:
```bash
pip install hrflow-connectors
```


To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :

<p align="center">

```python
from hrflow_connectors import SmartRecruiters
```

</p>

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/215806762-6249127e-cfe1-4b3f-944c-081aab631db7.jpeg width=90% height=100% >
</p>


Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [SmartRecruiters](https://www.smartrecruiters.com/) to learn more.
- ⚙️ API documentation : (https://developers.smartrecruiters.com/doc)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/smartrecruiters) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai : [the-forest-tree](https://github.com/the-forest-tree) - Software Engineer
- 🤝 [SmartRecruiters](https://www.smartrecruiters.com/) : Special thanks to the SmartRecruiters team for their help and collaboration in building this integrationn