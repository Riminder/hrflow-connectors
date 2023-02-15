# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Recruitee](#-about-recruitee)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Example](#-quick-start-example)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Recruitee

Recruitee is a business producing or selling computer "software as a service "(Saas.) The software functions as an applicant tracking system for handling applications for jobs. It includes a careers site editing system for employer branding, a plugin for sourcing (personnel) (otherwise called recruitment), employment website integration, email and calendar synchronization. 

<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/213731384-67e489da-26af-452b-8ca8-d280f5e74f82.jpeg width=90% height=100% >
</p>



# 📊 Data Flow 

In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">

<image src=https://user-images.githubusercontent.com/107410704/213734426-6ec72296-df58-4fa0-970c-389ec28d88f6.jpg width=90% height=100% >
</p>



# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- |  -------- |
| [**Push profile list**](docs/push_profile_list.md) | Writes a profile from Hrflow.ai Source as a candidate on Recruitee via the API| 
| [**Pull job list**](docs/pull_job_list.md) | Retrieves jobs via the ***Adzuna*** API Search endpoint and send them to a [HrFlow.ai](http://HrFlow.ai) Board|


</p>


# 🐍 Quick Start Example


To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/219021821-db7c92bc-b6d8-4256-bb5d-8c57bd846d3a.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Recruitee](https://recruitee.com) to learn more.
- ⚙️ API documentation : (https://docs.recruitee.com/reference/getting-started)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/recruitee) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai :  [Nedhir Ebnou](https://github.com/nedhirouebnou) - Software Engineer
- 🤝 Recruitee :[Recruitee for the partnership and accessible documentation](https://recruitee.com)

