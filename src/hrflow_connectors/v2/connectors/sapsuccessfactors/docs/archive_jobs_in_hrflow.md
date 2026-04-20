# Archive jobs in hrflow
`SAP SuccessFactors` :arrow_right: `HrFlow`

Send **archived** 'job(s)' _from_ SAP SuccessFactors _to_ HrFlow



## SAP SuccessFactors Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_server` :red_circle: | `string` | None | Server to be accessed |
| `api_key` :red_circle: | `string` | None | API Key used to authenticate on the SAP API |

## HrFlow.ai Auth Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `api_secret` :red_circle: | `string` | None | API Key used to access HrFlow.ai API |
| `api_user` :red_circle: | `string` | None | User email used to access HrFlow.ai API |

## Pull Parameters (SAP SuccessFactors)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `$top`  | `integer\|null` | 100 | Show only the first n items value is capped at 100 |
| `$skip`  | `integer\|null` | None | Skip the first n items |
| `$search`  | `string\|null` | None | Search items by search phrases |
| `$filter`  | `string\|null` | None | Filter items by property values |
| `$count`  | `boolean\|null` | None | Include count of items |
| `$orderby`  | `array\|null` | None | Order items by property values
Available values : accommodation_cost, accommodation_cost desc, age, age desc, agency_fee, agency_fee desc, annual_FB, annual_FB desc, annual_PF, annual_PF desc, annual_SA, annual_SA desc, annual_cashPay, annual_cashPay desc, annual_gratuity, annual_gratuity desc, annual_retirals, annual_retirals desc, appTemplateId, appTemplateId desc, assessRatingScaleName, assessRatingScaleName desc, candidateProgress, candidateProgress desc, city, city desc, classificationTime, classificationTime desc, classificationType, classificationType desc, closedDateTime, closedDateTime desc, comment, comment desc, commentEquip, commentEquip desc, commission, commission desc, corporatePosting, corporatePosting desc, costCenterId, costCenterId desc, costOfHire, costOfHire desc, country, country desc, createdDateTime, createdDateTime desc, currency, currency desc, customString10, customString10 desc, customString3, customString3 desc, customString4, customString4 desc, customString8, customString8 desc, defaultLanguage, defaultLanguage desc, deleted, deleted desc, department, department desc, division, division desc, erpAmount, erpAmount desc, facility, facility desc, formDataId, formDataId desc, formDueDate, formDueDate desc, function, function desc, hiringManagerNote, hiringManagerNote desc, industry, industry desc, instrCostHire, instrCostHire desc, instrERS, instrERS desc, instrEquest, instrEquest desc, instrEquipment, instrEquipment desc, instrInterview, instrInterview desc, instrInvolvedParties, instrInvolvedParties desc, instrManagernote, instrManagernote desc, instrOverallComments, instrOverallComments desc, instrPD, instrPD desc, instrReq, instrReq desc, instrReqDates, instrReqDates desc, instrReqDetails, instrReqDetails desc, instrReqJustify, instrReqJustify desc, instrReqRequire, instrReqRequire desc, instrSalary, instrSalary desc, instr_rec_cost, instr_rec_cost desc, internalStatus, internalStatus desc, intranetPosting, intranetPosting desc, isDraft, isDraft desc, jobCode, jobCode desc, jobReqGUId, jobReqGUId desc, jobReqId, jobReqId desc, jobRole, jobRole desc, jobStartDate, jobStartDate desc, lastModifiedBy, lastModifiedBy desc, lastModifiedDateTime, lastModifiedDateTime desc, lastModifiedProxyUserId, lastModifiedProxyUserId desc, legentity, legentity desc, location, location desc, misc_cost, misc_cost desc, monthly_FB, monthly_FB desc, monthly_PF, monthly_PF desc, monthly_SA, monthly_SA desc, monthly_cashPay, monthly_cashPay desc, monthly_gratuity, monthly_gratuity desc, monthly_retirals, monthly_retirals desc, monthly_salary, monthly_salary desc, numberOpenings, numberOpenings desc, openingsFilled, openingsFilled desc, otherBonus, otherBonus desc, otherCompensation, otherCompensation desc, otherEquip, otherEquip desc, othrComms, othrComms desc, overallScaleName, overallScaleName desc, positionNumber, positionNumber desc, postalcode, postalcode desc, ratedApplicantCount, ratedApplicantCount desc, recruitJust, recruitJust desc, replforwhom, replforwhom desc, restorehiringManagerTeamAdminDefaults, restorehiringManagerTeamAdminDefaults desc, restorerecruiterTeamAdminDefaults, restorerecruiterTeamAdminDefaults desc, restoresourcerTeamAdminDefaults, restoresourcerTeamAdminDefaults desc, restorevpOfStaffingTeamAdminDefaults, restorevpOfStaffingTeamAdminDefaults desc, reverseScale, reverseScale desc, salRateType, salRateType desc, salaryBase, salaryBase desc, salaryComment, salaryComment desc, salaryMax, salaryMax desc, salaryMin, salaryMin desc, stateProvince, stateProvince desc, statusSetId, statusSetId desc, stockPackage, stockPackage desc, targetBonusAmount, targetBonusAmount desc, tempDate, tempDate desc, templateId, templateId desc, templateType, templateType desc, timeToFill, timeToFill desc, total_earnings, total_earnings desc, total_fixed_pay, total_fixed_pay desc, total_hire_cost, total_hire_cost desc, travel_cost, travel_cost desc, workHours, workHours desc |

## Push Parameters (HrFlow)

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `board_key` :red_circle: | `string` | None | HrFlow.ai board key |

## Other Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
| `workflow_id` :red_circle: | `string` | None | A stable identifier used for persisting in incremental mode |
| `logics` :red_circle: | `array\|null` | None | A list of functions called in sequence with each item pulled from the origin. Each function might either return it's argument or None to discard the item. Any item discarded is eventually not pushed to the target |
| `format`  | `Callable\|null` | None | A formatting function to apply on items pulled before the push |
| `callback`  | `Callable\|null` | None | Registers a callback function to be called at the of a successful execution |
| `persist`  | `boolean` | True | When False has the effect of running in dry mode. Items are pulled but not pushed to the target |
| `incremental`  | `boolean` | False | Controls the incremental reading execution mode |

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors.v2 import SAPSuccessFactors


logging.basicConfig(level=logging.INFO)


SAPSuccessFactors.archive_jobs_in_hrflow(
    workflow_id=...,
    logics=...,
    connector_auth=dict(
        api_server=...,
        api_key=...,
    ),
    hrflow_auth=dict(
        api_secret=...,
        api_user=...,
    ),
    pull_parameters=dict(
        $top=...,
        $skip=...,
        $search=...,
        $filter=...,
        $count=...,
        $orderby=...,
    ),
    push_parameters=dict(
        board_key=...,
    ),
    format=...,
    callback=...,
    persist=...,
    incremental=...
)
```