# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Pôle emploi](#-about-pôle-emploi)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Example](#-quick-start-example)
- [🔗 Useful Links](#-useful-links)
- [👏 Special Thanks](#-special-thanks)


# 📝 About Pôle emploi

Access all the job offers available on the Pôle emploi website at any time and in real time.

<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/213744087-8967fb2a-ccb1-45c5-8375-04942b4c43e4.jpg width=90% height=100% >
</p>



# 📊 Data Flow

In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">

<image src=https://user-images.githubusercontent.com/107410704/213745057-c9d8a69d-d65f-4dd3-929c-5f5cbcabc1cc.jpg width=90% height=100% >
</p>



# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves jobs via the ***Offres d'emploi v2*** API from the Pôle emploi website based on selection criteria set in the and send them to a ***Hrflow.ai Board***. |


</p>


# 🐍 Quick Start Example


To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://user-images.githubusercontent.com/107410704/219022492-3fae3597-3713-4e91-ae69-91a5d05764d6.png width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers.

For more code details checkout connector code



# 🔗 Useful Links

- 📄Visit [Pôle emploi](https://www.pole-emploi.fr/accueil/) to learn more.
- ⚙️ API documentation : (https://pole-emploi.io/data/api/offres-emploi?tabgroup-api=documentation&doc-section=api-doc-section-rechercher-par-crit%C3%A8res)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/poleemploi) on our Github.


# 👏 Special Thanks
- 💻 HrFlow.ai :  [Nedhir Ebnou](https://github.com/nedhirouebnou) - Software Engineer
