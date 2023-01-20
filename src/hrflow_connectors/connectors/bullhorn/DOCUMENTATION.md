# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Bullhorn](#-about-bullhorn)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Bullhorn 

Bullhorn is a cloud computing company headquartered in Boston, Massachusetts. The company provides customer relationship management (CRM), applicant tracking system (ATS) and operations software for the staffing industry. As of 2019, the company reported more than 11,000 customers in more than 150 countries. Besides its Boston headquarters, the company has operations in St. Louis, London, Brighton, Sydney and Rotterdam.



<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213147416-a4e473ca-093b-47d6-82a0-5bfd0f67a46f.png width=90% height=100% >
</p>

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213159852-2d459dc6-cbb0-44e3-aa6a-a81a553b0aa2.jpg width=90% height=100% >
</p>

# 🔌 Connector Actions
<p align="center">


| Action | Description |
| ------- |  -------- |
| [**Pull jobs**](docs/pull_jobs.md) | Retrieves jobs from Bullhorn API  and sends them to a [HrFlow.ai](http://HrFlow.ai) Board |
| [**Pull profiles**](docs/push_profiles.md) | Retrieves profiles from Bullhorn ATS and sends them to a [HrFlow.ai](http://HrFlow.ai) Source|
| [**Push profiles**](docs/push_profiles.md) | Retrieves profiles from a HrFlow Source and sends them to Bullhorn ATS|

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
from hrflow_connectors import Bullhorn
```

</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Bullhorn](https://bullhorn.com/) to learn more.
- ⚙️ API documentation : (https://bullhorn.github.io/docs/)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/bullhorn) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai: [Daniel ROSA](https://github.com/DanielRosa73) - Software Engineer
- 💻 HrFlow.ai: [Limam VADHEL](https://github.com/limamvadhel) - Software Engineer
- 💻 HrFlow.ai: [Clement NEGRE](https://github.com/ClemNeg0) - Software Engineer
- 🤝 Bullhorn: [Bullhorn for the partnership and accessible documentation](https://bullhorn.com/)


