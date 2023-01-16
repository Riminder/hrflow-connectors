
# Workable Connector

> More than an applicant tracking system, Workable's talent acquisition software helps teams find candidates, evaluate applicants and make the right hire, faster.


## Connector features :

![BreezyHR(3)](https://user-images.githubusercontent.com/55802491/212697884-75682f55-3de4-40c1-9cc3-31955f7a9e9d.jpg)

- [**Pull jobs**](docs/pull_jobs.md) : Retrieves profiles from HrFlow Souce export API and sends them to Workable ATS


```py
import logging
from hrflow_connectors import Workable
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Workable.pull_jobs(
    workflow_id="some_string_identifier",
    action_parameters=dict(
        logics=[],
        format=lambda *args, **kwargs: None # Put your code logic here,
        read_mode=ReadMode.sync,
    ),
    origin_parameters=dict(
        auth="your_auth",
        subdomain="your_subdomain",
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
![image](https://user-images.githubusercontent.com/55802491/212356711-8edfc9df-bf96-4dda-825b-f5bf1f324edf.png)

- [**Push profile**](docs/push_profile.md)
 : Retrieves jobs from Workable vacancies export API and sends them to a HrFlow.ai Board

```py
import logging
from hrflow_connectors import Workable
from hrflow_connectors.core import ReadMode


logging.basicConfig(level=logging.INFO)


Workable.push_profile(
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
        auth="your_auth",
        subdomain="your_subdomain",
        shortcode="your_shortcode",
    )
)
```

![image](https://user-images.githubusercontent.com/55802491/210259130-5fc9d163-29aa-47ed-9d19-0559385e4edc.png)

| Actions |
| ------- |
| [**Pull jobs**](docs/pull_jobs.md) |
| [**Push profile**](docs/push_profile.md) |


## **Useful links:**

📄Visit [Workable](https://www.workable.com/) to learn more.

💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/workable) on our Github.
