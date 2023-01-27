# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About SmartRecruiters](#-about-smartrecruiters)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About SmartRecruiters

SmartRecruiters’ Talent Acquisition Suite is used by organizations to make the best hires. It has complete functionality for recruitment marketing and collaborative hiring built on a modern cloud platform with an open marketplace for 3rd party recruitment services.

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214608440-2d838ca1-cdd9-49ca-ac3b-25d65f06994a.png width=90% height=100% >
</p>

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214608101-4a496152-8aec-4528-afd0-b0712f9c5010.png width=90% height=100% >
</p>


# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/214609749-f47466cc-efe9-4132-b81b-dc0dcfe77464.png width=90% height=100% >
</p>

# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- |  -------- |
| [**Pull jobs**](docs/pull_jobs.md) | Retrieves all jobs from SmartRecruiters and sends them to an HrFlow board | 
| [**Pull profiles**](docs/pull_profiles.md) | Retrieves profiles from SmartRecruiters and sends them to an Hrflow.ai Source |
| [**Push profiles**](docs/push_profiles.md) | Sends profiles from an HrFlow Source to SmartRecruiters as a candidate for a given job |

</p>


# 🐍 Quick Start Examples

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**. To do this, execute the following steps in a new virtual environment:
```bash
pip hrflow-connectors
```


To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :

<p align="center">

```python
import logging
from hrflow_connectors import SmartRecruiters
```

</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [SmartRecruiters](https://www.smartrecruiters.com/) to learn more.
- ⚙️ API documentation : (https://developers.smartrecruiters.com/doc)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/smartrecruiters) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai : [Jamal GOURINDA](https://github.com/the-forest-tree) - Software Engineer
- 🤝 SmartRecruiters :[SmartRecruiters for the partnership and accessible documentation](https://www.smartrecruiters.com/)