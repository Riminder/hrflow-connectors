# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Waalaxy](#-about-waalaxy)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Example](#-quick-start-example)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About Waalaxy

Waalaxy (ex ProspectIn) allows you to automatically contact your prospects on different channels, mainly on LinkedIn and by email, via fully automated sequences.

<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/213688314-396051ba-7e04-4f8c-bede-9ca4a19bda07.jpg width=90% height=100% >
</p>


# 📊 Data Flow 

In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">

<image src=https://user-images.githubusercontent.com/107410704/213689474-b5846f29-9758-404e-9835-77b2ad11bc15.jpg width=90% height=100% >
</p>



# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- |  -------- |
| [**Catch profile trigger view**](docs/catch_profile_trigger_view.md) | Imports the visited profiles, in synchronization with the Waalaxy campaign (Visit + CRM Sync) | 
| [**Catch profile trigger connection**](docs/catch_profile_trigger_connection.md) | Imports the profiles just connected with, in synchronisation with the Waalaxy campaign (Visit + Invitation + CRM Sync)|

</p>


# 🐍 Quick Start Example

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**. To do this, execute the following steps in a new virtual environment:
```bash
pip hrflow-connectors
```


To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :

<p align="center">

```python
import logging
from hrflow_connectors import Waalaxy
```

</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [Waalaxy](https://www.waalaxy.com/) to learn more.
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/waalaxy) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai :  [Nedhir Ebnou](https://github.com/nedhirouebnou) - Software Engineer

