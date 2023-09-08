# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Ceridian](#-about-ceridian)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Ceridian 

Cloud HCM software that brings together real-time payroll, HR, benefits, time reporting, talent and workforce management to empower your people.

<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213708839-35e09e59-8535-423b-bd01-55c37b5fbddd.png width=90% height=100% >
</p>

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213160807-9df403fa-7891-4852-a8a6-d43a942bd629.jpg width=90% height=100% >
</p>


# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves all jobs via the ***Ceridian*** API and send them to an ***Hrflow.ai Board***. |


</p>


# 🐍 Quick Start Examples


To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/219010630-f7f92608-48a9-43ce-95fb-2157a436b3cd.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Ceridian](https://ceridian.com/) to learn more.
- ⚙️ API documentation : (https://developers.dayforce.com/Build/Download-documentation.aspx)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/ceridian) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai: [Daniel ROSA](https://github.com/DanielRosa73) - Software Engineer
- 💻 HrFlow.ai:  [Limam VADHEL](https://github.com/limamvadhel) - Software Engineer
- 💻 HrFlow.ai: [Corentin DUCHENE](https://github.com/CorentinDuchene) - Software Engineer
- 🤝 Ceridian: [Ceridian for the partnership and accessible documentation](https://ceridian.com/)
