# 📖 Summary

- [📖 Summary](#-summary)
- [💼 About Lever](#-about-lever)
- [🔧 How does it work?](#-how-does-it-work)
  - [📊 Data Flow:](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
- [🔗 Useful Links](#-useful-links)
- [👏 Special Thanks](#-special-thanks)

# 💼 About Lever

> Lever is a modern recruitment platform that helps companies streamline their hiring process.
<p align="center">
<image src= https://github.com/Riminder/hrflow-connectors/assets/135601200/84dd9011-1b4f-49ca-97f7-fa8674281b77 width=90% height=100% >
</p>


# 🔧 How does it work?

## 📊 Data Flow
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process:

# 🔌 Connector Actions

| Actions | Description |
| ------- | ----------- |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves all jobs via the Lever API and sends them to the Hrflow.ai Board. |
| [**Pull profile list**](docs/pull_profile_list.md) | Retrieves all profiles via the Lever API and sends them to the Hrflow.ai Board. |
| [**Push profile**](docs/push_profile.md) | Writes a profile from the Hrflow.ai Source to Lever via the API. |

# 🐍 Quick Start Examples

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.

To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


Once the connector module is imported, you can leverage all the different actions that it offers.

For more code details checkout connector code.

# 🔗 Useful Links

- 📄Visit [Lever](https://hire.lever.co/developer/documentation) to learn more.
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/lever) on our Github.

# 👏 Special Thanks

- 💻 HrFlow.ai : Mezid Abdellahi - Software Engineer
- 🤝 Lever : Integration Support
