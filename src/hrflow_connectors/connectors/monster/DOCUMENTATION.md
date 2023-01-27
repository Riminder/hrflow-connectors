# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Monster](#-about-monster)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Monster 

Monster is a career development and job search platform. It offers a wide range of services for job seekers, including resume and cover letter building tools, job search features, career advice, and more. Additionally, Monster provides recruiting and hiring solutions for employers, such as job posting, candidate screening, and applicant tracking tools. The platform is available in more than 40 countries and is available in multiple languages.

<p align="center">
<image src=https://user-images.githubusercontent.com/113343504/215118273-67b2dfd3-8d94-4f64-b89d-1c61381513bb.png width=90% height=100% >
</p>


# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

![image](https://user-images.githubusercontent.com/113343504/215128002-4b3b5d57-8059-40e9-b3a5-d37a7ada610c.png)

# 🔌 Connector Actions
<p align="center">


| Action | Description |
| ------- |  -------- |
| [**Push job**](docs/pull_jobs.md) | Retrieves job from Monster API  and sends them to a [HrFlow.ai](http://HrFlow.ai) Board |
| [**Catch profile**](docs/push_profiles.md) | Retrieves profile from Monster ATS and sends them to a [HrFlow.ai](http://HrFlow.ai) Source|

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
from hrflow_connectors import Monster
```

</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code

# 🔗 Useful Links

- 📄Visit [Monster](https://www.monster.com/) to learn more.
- ⚙️ API documentation : (http://integrations.monster.com/doc/)
- ⚙️ toolkit for testing json: (https://integrations.monster.com/Toolkit/)
- To see job pushed: (http://jobview.monster.com/getjob.aspx?jobid=xxx) with xxx is jobposting id, findable in response. (see picture below)
<image src=https://user-images.githubusercontent.com/113343504/209932759-ba04c5f5-ba39-45cb-a54d-3d3e49a3d1f1.png>



# 🙏 Special Thanks  
- 💻 HrFlow.ai: [Arthur STIEVENARD](https://github.com/arthurstiev) - Software Engineer
- 💻 HrFlow.ai: [Limam VADHEL](https://github.com/limamvadhel) - Software Engineer
- 💻 HrFlow.ai: [Clement NEGRE](https://github.com/ClemNeg0) - Software Engineer
- 🤝 Monster

