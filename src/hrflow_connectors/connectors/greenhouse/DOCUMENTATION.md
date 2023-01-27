# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Greenhouse](#-about-greenhouse)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Greenhouse 

Greenhouse is an applicant tracking system and recruitment software that helps companies manage and streamline their hiring process. It includes features such as job posting, candidate tracking, scheduling interviews, and offer management.  Greenhouse also provides analytics and reporting to help companies track the effectiveness of their recruitment process. It also offers an API to help companies integrating it with other internal systems.



<p align="center">
<image src=https://user-images.githubusercontent.com/113343504/215117041-ebbc3963-5fbe-4706-b9c7-97882680fac0.png
 width=90% height=100% >
</p>


# 🔌 Connector Actions
<p align="center">


| Action | Description |
| ------- |  -------- |
| [**Pull jobs**](docs/pull_jobs.md) | Retrieves jobs from Greenhouse API  and sends them to a [HrFlow.ai](http://HrFlow.ai) Board |
| [**Push profiles**](docs/push_profiles.md) | Retrieves profiles from a [HrFlow.ai](http://HrFlow.ai) Source and sends them to Greenhouse ATS|

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
from hrflow_connectors import Greenhouse
```

</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Greenhouse](https://greenhouse.com/) to learn more.
- ⚙️ API documentation : (https://developers.greenhouse.io/harvest.html#candidates)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/greenhouse) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai: [Arthur STIEVENARD](https://github.com/arthurstiev) - Software Engineer
- 💻 HrFlow.ai: [Limam VADHEL](https://github.com/limamvadhel) - Software Engineer
- 💻 HrFlow.ai: [Clement NEGRE](https://github.com/ClemNeg0) - Software Engineer
- 🤝 Greenhouse: [Greenhouse for the partnership and accessible documentation](https://greenhouse.com/)
