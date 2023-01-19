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
## **Push Profiles Action**

<p align="center">

```python
import logging
from hrflow_connectors import BreezyHR
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


BreezyHR.push_profiles(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        profile_key="your_profile_key",
    ),
    target_parameters=dict(
        email="your_email",
        password="your_password",
        company_id="your_company_id",
        company_name="your_company_name",
        position_id="your_position_id",
        origin=sourced,
    )
)
```

</p>

## **Pull Jobs Action**

<p align="center">

```python
import logging
from hrflow_connectors import BreezyHR
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


BreezyHR.pull_jobs(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        email="your_email",
        password="your_password",
        company_id="your_company_id",
        company_name="your_company_name",
    ),
    target_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key",
        sync=True,
        update_content=False,
        enrich_with_parsing=False,
    )
)
```
</p>
# 🔗 Useful Links

- 📄Visit [BreezyHR](https://breezy.hr/) to learn more.
- ⚙️ API documentation : (https://developer.breezy.hr/reference/overview)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/breezyhr) on our Github.


# 🙏 Special Thanks  
- 💻 [Limam VADHEL](https://github.com/limamvadhel)
- 💻 [Leo FERRETTI](https://github.com/Sprenger07)
- 💻 [Corentin DUCHENE](https://github.com/CorentinDuchene)
- 🤝 [Breezy HR for the partnership and accessible documentation](https://breezy.hr/)

