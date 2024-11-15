# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Hubspot](#-about-hubspot)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Example](#-quick-start-example)
- [🔗 Useful Links](#-useful-links)
- [👏 Special Thanks](#-special-thanks)


# 📝 About Hubspot

HubSpot is a CRM platform with all the software, integrations, and resources you need to connect marketing, sales, content management, and customer service.


<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/213674431-f11f9805-6c43-456b-bb3d-b60ebf698415.jpg width=90% height=100% >
</p>


# 📊 Data Flow
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">

<image src=https://user-images.githubusercontent.com/107410704/213676477-70dd9009-f41b-4659-a58c-41f53a92f7d4.jpg width=90% height=100% >
</p>



# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Push profile**](docs/push_profile.md) | Writes a profile from Hrflow.ai Source as a contact on Hubspot via the API |
| [**Pull profile list**](docs/pull_profile_list.md) | Retrieves contacts from Hubspot via API and send them to a ***Hrflow.ai Source***. |


</p>


# 🐍 Quick Start Example


To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/219017227-8e7f86a6-fd8d-4e31-ab07-5ef13184eab0.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers.

For more code details checkout connector code


# 🔗 Useful Links

- 📄Visit [Hubspot](https://www.hubspot.com) to learn more.
- ⚙️ API documentation : (https://developers.hubspot.com/docs/api/overview)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/hubspot) on our Github.


# 👏 Special Thanks
- 💻 HrFlow.ai :  [Nedhir Ebnou](https://github.com/nedhirouebnou) - Software Engineer
- 🤝 Hubspot :[Hubspot for the  accessible documentation](https://www.hubspot.com)

