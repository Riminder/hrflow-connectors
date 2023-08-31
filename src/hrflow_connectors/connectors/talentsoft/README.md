# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Cegid-Talentsoft](#-about-cegid-talentsoft)
- [🔧 How does it work?](#-how-does-it-work)
  - [📊 Data integration capabilities:](#-data-integration-capabilities)
  - [🧠 Artificial Intelligence capabilities:](#-artificial-intelligence-capabilities)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Cegid-Talentsoft
Talentsoft (by Cegid) offers a full-suite HCM solution so you can keep all your HR processes in one platform. Make HR processes easy & simple for your teams. Help HR & IT leaders transform organizations. Accelerate HCM productivity for better performance.

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214645377-41f926d2-207a-47c7-bd92-e2a561200543.png width=90% height=100% >
</p>

#  🔧 How does it work?
## 📊 Data integration capabilities:
- ⬅️ Send Profiles data from Cegid Talentsoft to a Destination of your choice.
- ⬅️ Send Jobs data from Cegid Talentsoft to a Destination of your choice.


## 🧠 Artificial Intelligence capabilities:
- Extract, Structure, and Categorize Talent & Workforce data
- Search, Score, and Match Profiles & Jobs with our APIs and AI Widgets


# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Applicant new**](docs/applicant_new.md) | Handle TalentSoft 'applicant_new' event by fetching profile from TalentSoft and sending it to HrFlow.ai Parsing API. |
| [**Applicant resume update**](docs/applicant_resume_update.md) | Handle TalentSoft 'applicant_resume_update' event by running a new HrFlow.ai Parsing on updated resume. |
| [**Applicant update**](docs/applicant_update.md) | Handle TalentSoft 'applicant_update' event by only updating tags coming from TalentSoft in HrFlow.ai. |
| [**Pull profile list**](docs/pull_profile_list.md) | Retrieves profiles from TalentSoft candidates export API and send them to a ***Hrflow.ai Source***. |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves jobs from TalentSoft vacancies export API and send them to a ***Hrflow.ai Board***. |


</p>
<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214648408-fa861623-e059-4e7b-bd56-e09f85e30062.png width=90% height=100% >
</p>

# 🐍 Quick Start Examples

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**. 


To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/215837925-ccc3f4c7-8b6e-4712-81ee-5574f458b51f.png width=90% height=100% >
</p>


Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Cegid Talentsoft](https://www.cegidtalentsoft.com/) to learn more.
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/talentsoft) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai : [the-forest-tree](https://github.com/the-forest-tree) - Software Engineer
- 🤝 Cegid Talentsoft :[Cegid Talentsoft for the partnership and accessible documentation](https://www.cegidtalentsoft.com/)