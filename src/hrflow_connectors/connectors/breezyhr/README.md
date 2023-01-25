# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About BreezyHR](#-about-breezyhr)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
  - [**Push Profiles Action**](#push-profiles-action)
  - [**Pull Jobs Action**](#pull-jobs-action)
- [🔗 Useful Links](#-useful-links)
- [🙏 Special Thanks](#-special-thanks)


# 📝 About BreezyHR 

BreezyHr is a software designed to assist teams in finding the right candidates, evaluating applicants, and making a hire more quickly.
It helps streamline the recruitment process and identify the best fit for a position.


<p align="center">
<image src=https://user-images.githubusercontent.com/55802491/212358414-f29104b6-c54b-4f91-b376-1fe7b5fb8eb1.png width=90% height=100% >
</p>

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">
<image src=https://user-images.githubusercontent.com/55802491/212667728-8a1d7eab-04b0-453b-9381-444ff47751cd.jpg width=90% height=100% >
</p>

# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- |  -------- |
| [**Pull jobs**](docs/pull_jobs.md) | Retrieves profiles from HrFlow Souce export API and sends them to BreezyHR ATS | 
| [**Push profiles**](docs/push_profiles.md) | Retrieves jobs from BreezyHR vacancies export API  and sends them to a [HrFlow.ai](http://HrFlow.ai) Board|

</p>


# 🐍 Quick Start Examples
To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**. To do this, execute the following steps in a new virtual environment:
```bash
pip hrflow-connectors
```


To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :

<p align="center">

```python
import logging
from hrflow_connectors import BreezyHR
```

</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 


# 🔗 Useful Links

- 📄Visit [BreezyHR](https://breezy.hr/) to learn more.
- ⚙️ API documentation : (https://developer.breezy.hr/reference/overview)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/breezyhr) on our Github.


# 🙏 Special Thanks  
- 💻 HrFlow.ai :  [Limam VADHEL](https://github.com/limamvadhel) - Software Engineer
- 💻 HrFlow.ai : [Leo FERRETTI](https://github.com/Sprenger07) - Software Engineer
- 💻 HrFlow.ai :[Corentin DUCHENE](https://github.com/CorentinDuchene) - Software Engineer
- 🤝 BreezyHR :[Breezy HR for the partnership and accessible documentation](https://breezy.hr/)

