import typing as t


def _workday_job_location_get(workday_location: t.Dict) -> t.Dict:
    text = workday_location["descriptor"]
    for key in ["region", "country"]:
        val = workday_location[key]
        if not val:
            continue
        des = workday_location[key][val]["descriptor"]
        if des:
            text += f", {des}"
    hrflow_location = dict(text=text)
    return hrflow_location


def _workday_job_tags_get(workday_job: t.Dict) -> t.List[t.Dict]:
    T = []
    if workday_job["remoteType"]:
        value = workday_job["remoteType"]["name"]
        if value:
            T.append(dict(name="remoteType", value=value))
    if workday_job["categories"]:
        for ii, category in enumerate(workday_job["categories"]):
            T.append(dict(name=f"category{ii}", value=category["descriptor"]))
    if workday_job["spotlightJob"] is not None:
        T.append(dict(name="spotlightJob", value=workday_job["spotlightJob"]))
    for key in ["timeType", "jobType"]:
        if workday_job[key]:
            des = workday_job[key]["descriptor"]
            if des:
                T.append(dict(name=key, value=des))
    return T


def _workday_job_metadatas_get(workday_job: t.Dict) -> t.List[t.Dict]:
    M = []
    if workday_job["additionalLocations"]:
        for ii, location in enumerate(workday_job["additionalLocations"]):
            value = _workday_job_location_get(location).get("text")
            M.append(dict(name=f"additionalLocation{ii}", value=value))
    for key in ["company", "jobSite"]:
        if workday_job[key]:
            des = workday_job[key]["descriptor"]
            if des:
                M.append(dict(name=key, value=des))
    return M


def _workday_ranges_date_get(workday_job: t.Dict) -> t.List[t.Dict]:
    R = []
    if workday_job["endDate"] and workday_job["startDate"]:
        R.append(
            dict(
                name="jobPostingDates",
                value_min=workday_job["startDate"],
                value_max=workday_job["endDate"],
            )
        )
    return R
