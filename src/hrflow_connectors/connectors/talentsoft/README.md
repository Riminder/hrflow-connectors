# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Cegid-Talentsoft](#-about-cegid-talentsoft)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Cegid-Talentsoft
Talentsoft (by Cegid) offers a full-suite HCM solution so you can keep all your HR processes in one platform. Make HR processes easy & simple for your teams. Help HR & IT leaders transform organizations. Accelerate HCM productivity for better performance.

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214645377-41f926d2-207a-47c7-bd92-e2a561200543.png width=90% height=100% >
</p>

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process
  
<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214648408-fa861623-e059-4e7b-bd56-e09f85e30062.png width=90% height=100% >
</p>

# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- |  -------- |
| [**Pull jobs**](docs/pull_jobs.md) | Retrieves all jobs from Cegid Talentsoft and sends them to an HrFlow board | 
| [**Pull profiles**](docs/pull_profiles.md) | Retrieves profiles from Cegid Talentsoft and sends them to an Hrflow.ai Source |
| [**Catch profiles**](docs/catch_profiles.md) | Receives profiles. It catches three different events: applicant_new, applicant_update, and applicant_resume_update |

</p>


# 🐍 Quick Start Examples

To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :

<p align="center">

```python
import logging
from hrflow_connectors import TalentSoft
```

</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Cegid Talentsoft](https://www.cegidtalentsoft.com/) to learn more.
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/talentsoft) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai : [Jamal GOURINDA](https://github.com/the-forest-tree) - Software Engineer
- 🤝 Cegid Talentsoft :[Cegid Talentsoft for the partnership and accessible documentation](https://www.cegidtalentsoft.com/)