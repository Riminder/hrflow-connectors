# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About SAP](#-about-sap)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About SAP 

Get software and technology solutions from SAP, the leader in business applications. Run simple with the best in cloud, analytics, mobile and IT solutions.

<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213727218-85872de6-f244-4c60-8904-ae9082fe8aa8.png width=90% height=100% >
</p>

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">
<image src=https://user-images.githubusercontent.com/46778695/213727477-f6ab4372-5e1e-43c7-a574-bfe563feb352.jpg width=90% height=100% >
</p>

# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves all jobs via the ***SAPSuccessFactors*** API and sends them to a ***Hrflow.ai Board***. |
| [**Push profile**](docs/push_profile.md) | Writes a profile taken from a Hrflow.ai Source to SAPSuccessFactors via the SAP API |


</p>


# 🐍 Quick Start Examples

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/219327857-d3f3ec48-38cc-466a-ad48-7dce0c37e7ad.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [SAP](https://sap.com/) to learn more.
- ⚙️ API documentation : (https://api.sap.com/)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/sapsuccessfactors) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai: [Daniel ROSA](https://github.com/DanielRosa73) - Software Engineer
- 💻 HrFlow.ai: [Limam VADHEL](https://github.com/limamvadhel) - Software Engineer
- 💻 HrFlow.ai: [Corentin DUCHENE](https://github.com/CorentinDuchene) - Software Engineer
- 🤝 SAP: [SAP for the partnership and accessible documentation](https://sap.com)
