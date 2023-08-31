
- [💼  About Taleez](#--about-taleez)
- [😍 **What makes this major news for Taleez customers \& partners?**](#-what-makes-this-major-news-for-taleez-customers--partners)
- [🔧 How does it work?](#-how-does-it-work)
  - [📊 Data integration capabilities:](#-data-integration-capabilities)
  - [🧠 Artificial Intelligence capabilities:](#-artificial-intelligence-capabilities)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)

# 💼  About Taleez 

Taleez is a recruitment management software that helps small and medium-sized enterprises (SMEs) and intermediate-sized enterprises (ETIs) modernize their recruitment process while reducing costs.

With Taleez, HR teams can easily manage and centralize job applications, post job offers on multiple job boards, collaborate on recruitment management, and enhance the employer brand.


<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/223753124-44c05816-dbb2-47a2-9f7e-222fc9a67f98.png width=90% height=100% >
</p>

# 😍 **What makes this major news for Taleez customers & partners?**

This new connector will enable Taleez customers & partners to:

- ⚡ Seamlessly Integrate Talent & Workforce data
- 🤖 Elevate their Talent and Recruiter Experiences  
  
# 🔧 How does it work?
  
## 📊 Data integration capabilities:

- ➡️ Send Profiles data from a Source of your choice to Taleez
- ⬅️ Send Jobs data from Taleez to a Destination of your choice

## 🧠 Artificial Intelligence capabilities:
  
- Easily Extract, Structure, and Categorize Talent & Workforce data
- Effortlessly Search, Score, and Match Profiles and Jobs with precision.

# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Push profile**](docs/push_profile.md) | Retrieves a profile from HrFlow Source and posts it to Taleez ATS enriching it with properties extracted from the profile |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves all jobs via the ***Taleez*** API and send them to a ***Hrflow.ai Board***. |


</p>


# 🐍 Quick Start Examples


To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/223755373-312181ef-bcf2-45f4-b6d9-da74bac2339a.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 

# 🔗 Useful Links

- 📄Visit [Taleez](https://taleez.com/) to learn more.
- ⚙️ API documentation : (https://api.taleez.com/swagger-ui/index.html)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/taleez) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai :  [Yasser BELMEHDI](https://github.com/yass1337) - Software Engineer
- 🤝 Taleez : Fabien RIGOLLIER and all the Taleez team for their help and collaboration in building this integration
 