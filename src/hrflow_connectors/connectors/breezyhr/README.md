
# Breezyhr Connector

> BreezyHr is a software designed to assist teams in finding the right candidates, evaluating applicants, and making a hire more quickly.
It helps streamline the recruitment process and identify the best fit for a position.

## Connector features :

![BreezyHR](https://user-images.githubusercontent.com/55802491/212667728-8a1d7eab-04b0-453b-9381-444ff47751cd.jpg)


- **[Push profiles :](https://github.com/Riminder/hrflow-connectors/blob/master/src/hrflow_connectors/connectors/breezyhr/docs/pull_jobs.md)**  Retrieves profiles from HrFlow Souce export API and sends them to BreezyHR ATS

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
        origin="sourced",
    )
)
```
![image](https://user-images.githubusercontent.com/55802491/212358414-f29104b6-c54b-4f91-b376-1fe7b5fb8eb1.png)


- **[Pull Jobs :](https://github.com/Riminder/hrflow-connectors/blob/master/src/hrflow_connectors/connectors/breezyhr/docs/pull_jobs.md)** Retrieves jobs from BreezyHR vacancies export API  and sends them to a [HrFlow.ai](http://HrFlow.ai) Board


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
![image](https://user-images.githubusercontent.com/55802491/210258882-e9e0abda-62a5-4267-89f0-61460c10abe1.png)



| Actions |
| ------- |
| [**Pull jobs**](docs/pull_jobs.md) |
| [**Push profiles**](docs/push_profiles.md) |

## **Useful links:**

📄Visit [BreezyHR](https://breezy.hr/) and [BreezyHR API](https://developer.breezy.hr/reference/overview) to learn more.

💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/breezyhr) on our Github.





