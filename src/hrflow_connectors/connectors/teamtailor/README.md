# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Teamtailor](#-about-teamtailor)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Teamtailor

Teamtailor is the applicant tracking system made for all types of companies. With modern features optimized for you and your candidates, you will get everything you need to recruit successfully.



<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213733804-e5383200-1371-493f-854f-ea70d80a1e8c.png width=90% height=100% >
</p>

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213188767-7ddef358-0f0b-4d68-a14f-f0dcc42e4851.jpg width=90% height=100% >
</p>

 
# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Pull job list**](docs/pull_job_list.md) | Retrieve all jobs via the ***Teamtailor*** API and send them to an ***Hrflow.ai Board***. |
| [**Push profile**](docs/push_profile.md) | Writes a profile from an Hrflow.ai Source to Teamtailor via the API |


</p>


# 🐍 Quick Start Examples

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/219012378-809a6872-fc1a-45ed-917b-c88aad89de17.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Teamtailor](https://teamtailor.com/) to learn more.
- ⚙️ API documentation : (https://docs.teamtailor.com/)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/teamtailor) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai :  [Daniel ROSA](https://github.com/DanielRosa73) - Software Engineer
- 🤝 Teamtailor :[Teamtailor for the partnership and accessible documentation](https://teamtailor.com/)
