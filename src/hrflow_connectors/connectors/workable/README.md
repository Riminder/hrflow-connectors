# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Workable](#-about-workable)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Workable

Workable is more than an applicant tracking system, Workable's talent acquisition software helps teams find candidates, evaluate applicants and make the right hire, faster.


<p align="center">
<image src=https://user-images.githubusercontent.com/55802491/212356711-8edfc9df-bf96-4dda-825b-f5bf1f324edf.png width=90% height=100% >
</p>

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">
<image src=https://user-images.githubusercontent.com/55802491/214537920-a1937578-039f-4619-8533-72759ccab258.jpg width=90% height=100% >
</p>

# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves all jobs via the ***Workable*** API and send them to a ***Hrflow.ai Board***. |
| [**Push profile**](docs/push_profile.md) | Writes a profile from ***Hrflow.ai Source*** to ***Workable*** via the API for the given `shortcode`. |


</p>


# 🐍 Quick Start Examples

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/219015552-1ca1c524-3321-4f3e-81ee-b1ada3f675c4.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Workable](https://www.workable.com/) to learn more.
- ⚙️ API documentation : (https://workable.readme.io/reference/generate-an-access-token)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/workable) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai :  [Limam VADHEL](https://github.com/limamvadhel) - Software Engineer
- 💻 HrFlow.ai : [Leo FERRETTI](https://github.com/Sprenger07) - Software Engineer
- 💻 HrFlow.ai :[Corentin DUCHENE](https://github.com/CorentinDuchene) - Software Engineer
- 🤝 Workable :[Workable for the partnership and accessible documentation](https://www.workable.com/)
