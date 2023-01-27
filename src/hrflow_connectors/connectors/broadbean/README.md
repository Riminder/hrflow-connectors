# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Broadbean](#-about-broadbean)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Broadbean

Broadbean is a recruiting software company that provides a range of tools for job posting, candidate management, and applicant tracking. Their software allows users to post job listings to multiple job boards and social media sites, track applicant progress through the hiring process, and manage communication with candidates. Some of the features of Broadbean include resume parsing, automated job posting, and integration with other HR software.

<p align="center">
<image src=https://user-images.githubusercontent.com/113343504/215106838-2a2467b5-0b89-46e6-91cb-9097b5bdee51.png width=90% height=100% >
</p>

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

![image](https://user-images.githubusercontent.com/113343504/215125424-49eadcc1-6b1f-4c3b-8f00-19a874440ac1.png)




# 🔌 Connector Actions



| Action | Description |
| ------- |  -------- |
| [**Catch profiles**](docs/push_profiles.md) | Retrieves profiles from Broadbean ATS and sends them to a [HrFlow.ai](http://HrFlow.ai) Source|
| [**Push profile**](docs/push_profiles.md) | Retrieves profiles from a [HrFlow.ai](http://HrFlow.ai) Source and sends them to Broadbean ATS|

</p>


# 🐍 Quick Start Examples

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**. To do this, execute the following steps in a new virtual environment:
```bash
pip hrflow-connectors
```


To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


```python
import logging
from hrflow_connectors import Broadbean
```



</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Broadbean](https://broadbean.com/) to learn more.
- ⚙️ API documentation : (https://integrations.broadbean.com/hc/en-us/sections/115001362409-Candidate-Hub)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/broadbean) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai: [Stievenard Arthur](https://github.com/arthurstiev) - Software Engineer
- 🤝 Bullhorn: [Broadbean for the partnership and accessible documentation](https://broadbean.com/)
