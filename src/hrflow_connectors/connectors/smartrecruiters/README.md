# Smart Recruiters Connector
> Move beyond applicant tracking systems (ATS) with an enterprise-grade recruiting platform designed for the modern workforce. SmartRecruiters' Talent Acquisition Suite provides everything needed to attract, select, and hire great talent.

🔗 https://www.smartrecruiters.com/

## How to use

```python
import logging
from hrflow_connectors import SmartRecruiters


logging.basicConfig(level=logging.INFO)


SmartRecruiters.pull_jobs(
    action_parameters=dict(),
    source_parameters=dict(
        x_smart_token="your_x_smart_token",
        posting_status="PUBLIC"
    ),
    destination_parameters=dict(
        sync=True,
        enrich_with_parsing=True,
        update_content=False,
        api_secret="your_api_secret",
        api_user="your_api_user",
        board_key="your_board_key"
    )
)

SmartRecruiters.push_profile(
    action_parameters=dict(),
    source_parameters=dict(
        api_secret="your_api_secret",
        api_user="your_api_user",
        source_key="your_source_key",
        profile_key="your_profile_key",
    ),
    destination_parameters=dict(
        x_smart_token="your_x_smart_token",
        job_id="your_job_id",
    ),
)
```