# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Adzuna](#-about-adzuna)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Example](#-quick-start-example)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Adzuna

Find Every Job, Everywhere with Adzuna

Adzuna is a smarter, more transparent job search engine that helps you dodge the thousands of irrelevant jobs so you can zero in on the right role faster.

<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/213695245-0182ea76-a5ff-4784-a13f-bdd5c80b84f9.png width=90% height=100% >
</p>


# 📊 Data Flow 

In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">

<image src=https://user-images.githubusercontent.com/107410704/213694243-8503e06d-e17d-4094-9984-e419341619e2.jpg width=90% height=100% >
</p>



# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves jobs via the ***Adzuna'*** API Search endpointand send them to a ***Hrflow.ai Board***. |


</p>


# 🐍 Quick Start Example

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/57711045/215874507-a36933f2-af97-4ee1-83d4-1d08eeebe06b.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Adzuna](https://www.adzuna.fr) to learn more.
- ⚙️ API documentation : (https://developer.adzuna.com/docs/search)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/adzuna) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai :  [Nedhir Ebnou](https://github.com/nedhirouebnou) - Software Engineer



