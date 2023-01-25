# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Workable](#-about-workable)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
  - [**Push Profiles Action**](#push-profiles-action)
  - [**Pull Jobs Action**](#pull-jobs-action)
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
| ------- |  -------- |
| [**Pull jobs**](docs/pull_jobs.md) | Retrieves profiles from HrFlow Souce export API and sends them to Workable ATS | 
| [**Push profiles**](docs/push_profiles.md) | Retrieves jobs from Workable vacancies export API  and sends them to a [HrFlow.ai](http://HrFlow.ai) Board|

</p>


# 🐍 Quick Start Examples
## **Push Profiles Action**

To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**. To do this, execute the following steps in a new virtual environment:
```bash
pip install hrflow-connectors
```


To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :

<p align="center">

```python
import logging
from hrflow_connectors import Workable
```

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