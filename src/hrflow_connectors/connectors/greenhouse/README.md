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

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

![image](https://user-images.githubusercontent.com/113343504/215127610-4936bffa-3262-4e72-942b-43ba22dd2ccd.png)

# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves all jobs of a board via the ***Greenhouse*** API and send them to a ***Hrflow.ai Board***. |
| [**Push profile**](docs/push_profile.md) | Writes a profile from Hrflow.ai Source to Greenhouse  via the API for the given job_id(s) provided in tags. |


</p>


# 🐍 Quick Start Examples


To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/219013506-7fb78971-ee1e-4374-8d89-6956034093c3.png width=90% height=100% >
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
