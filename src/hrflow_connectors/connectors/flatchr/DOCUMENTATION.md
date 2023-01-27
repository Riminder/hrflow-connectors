# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Bullhorn](#-about-flatchr)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Flatchr 

Flatchr is a web-based Applicant Tracking System (ATS) and recruitment software. It is designed to help businesses and recruiters manage the recruitment process, from job posting and applicant tracking, to scheduling interviews and managing offers. It has features like candidate management, resume parser, job postings, sourcing, and more. 



<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213147416-a4e473ca-093b-47d6-82a0-5bfd0f67a46f.png width=90% height=100% >
</p>


# 🔌 Connector Actions
<p align="center">


| Action | Description |
| ------- |  -------- |
| [**Push profiles**](docs/push_profiles.md) | Retrieves profiles from a [HrFlow.ai](http://HrFlow.ai) Source and sends them to Bullhorn ATS|

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
from hrflow_connectors import Flatchr
```

</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Bullhorn](https://www.flatchr.io/) to learn more.
- ⚙️ API documentation : (https://developers.flatchr.io/docs/getting_started)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/flatchr) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai: [Arthur STIEVENARD](https://github.com/arhturstiev) - Software Engineer
- 💻 HrFlow.ai: [Limam VADHEL](https://github.com/limamvadhel) - Software Engineer
- 💻 HrFlow.ai: [Clement NEGRE](https://github.com/ClemNeg0) - Software Engineer
- 🤝 Flatchr: [Flatchr for the partnership and accessible documentation](https://flatchr.com/)