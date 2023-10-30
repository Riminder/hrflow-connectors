# 📖 Summary
- [📖 Summary](#-summary)
- [📝 About Salesforce](#-about-salesforce)
- [📊 Data Flow](#-data-flow)
- [🔌 Connector Actions](#-connector-actions)
- [🐍 Quick Start Examples](#-quick-start-examples)
  - [**Push Profiles Action**](#push-profiles-action)
  - [**Pull Jobs Action**](#pull-jobs-action)
- [🔗 Useful Links](#-useful-links)
- [👍 Special Thanks](#-special-thanks)


# 📝 About Salesforce 

Salesforce, Inc. is an American cloud-based software company headquartered in San Francisco, California. It provides customer relationship management (CRM) software and applications focused on sales, customer service, marketing automation, e-commerce, analytics, and application development.


<p align="center">
<image src=https://github.com/Riminder/hrflow-connectors/assets/135601200/a331919a-004b-4093-aadb-48ab688b177b width=90% height=100% >
</p>

# Prerequesit

In order to leverage the Salesforce connector you need to perform initial setup of HrFlow tables in your Salesforce integration. 
The code below uses Salesforce API to create the following Salesforce Custom Objects: 
- `Hrflow_Job__c`
- `Hrflow_Profile__c`
    - `Hrflow_Experience__c`
    - `Hrflow_Education__c`
    - `Hrflow_Attachment__c`

> 💡 _Before running the code you can review all details about the Custom Objects in this [JSON](./hrflow_custom_objects.json) file_

In order to run code below you need: 
- API Access enabled for your organization. Check this [link](https://help.salesforce.com/s/articleView?id=000385436&type=1) to see if it is supported by your Salesforce Edition
- A Salesforce user with API Enabled permission and enough privileges to create Custom Objects
- Install the [simple-salesforce](https://github.com/simple-salesforce/simple-salesforce) Python package 
```python
import json

from simple_salesforce import Salesforce

HRFLOW_CUSTOM_OBJECTS_PATH = "./hrflow_custom_objects.json"

# Salesforce credentials
username = "==============FILL==ME=============="
password = "==============FILL==ME=============="
# How Can I Find My Security Token and Use It in Data Loader | Salesforce Platform 
# https://www.youtube.com/watch?v=nYbfxeSGKFM&ab_channel=SalesforceSupport
security_token = "==============FILL==ME=============="
# Find your Salesforce Organization ID 
# https://help.salesforce.com/s/articleView?id=000385215&type=1
organization_id = "==============FILL==ME=============="

sf = Salesforce(username=username, password=password, security_token=security_token, organizationId=organization_id)
mdapi = sf.mdapi

with open(HRFLOW_CUSTOM_OBJECTS_PATH, "r") as f:
    hrflow_custom_objects = json.load(f)

for custom_object  in hrflow_custom_objects:
    _custom_object = mdapi.CustomObject(
        fullName = custom_object["object_name"],
        label = custom_object["label"],
        pluralLabel = custom_object["pluralLabel"],
        nameField = mdapi.CustomField(
            label = "Name",
            type = mdapi.FieldType("Text")
        ),
        fields=[
            mdapi.CustomField(
                **field,
            ) for field in custom_object["fields"]
        ],
        deploymentStatus = mdapi.DeploymentStatus("Deployed"),
        sharingModel = mdapi.SharingModel("ReadWrite")
    )
    mdapi.CustomObject.create(_custom_object)
```

## How to create list view of HrFlow Custom Objects
Once logged to your Salesforce environment go to Setup then click Create > Custom Tab. Once created you should be able to view the list of records in each of the tables. 

<image width="1919" alt="HrFlow Jobs" src="https://github.com/Riminder/hrflow-connectors/assets/65894619/f20f62fd-e370-4e7f-8b85-0c4c2d4e43d1">
<image width="1919" alt="HrFlow Profiles" src="https://github.com/Riminder/hrflow-connectors/assets/65894619/828f25f5-a3da-4a4f-bcbb-56d6f75c3304">
<image width="1919" alt="HrFlow Educations" src="https://github.com/Riminder/hrflow-connectors/assets/65894619/7979be9c-ebf2-4f84-ba79-ab54224f4a67">
<image width="1919" alt="HrFlow Experiences" src="https://github.com/Riminder/hrflow-connectors/assets/65894619/3006d23f-2944-4e8e-89b1-c6b58a49de21">
<image width="1919" alt="HrFlow Attachments" src="https://github.com/Riminder/hrflow-connectors/assets/65894619/dfcdba59-658b-4a75-870b-99c991f53935">

# 📊 Data Flow 
In this section, we outline the data flow between different components of the connector. The following schema provides a graphical representation of the data exchange process

<p align="center">
<image src=https://github.com/Riminder/hrflow-connectors/assets/65894619/6550ac4f-bc62-47e5-9340-a1475e612bb9 width=90% height=100% >
</p>

# 🔌 Connector Actions
<p align="center">

| Action | Description |
| ------- | ----------- |
| [**Pull profile list**](docs/pull_profile_list.md) | Retrieves profiles from Salesforce HrFlow Profile & Co Custom Objects and writes them to an Hrflow.ai source |
| [**Push profile**](docs/push_profile.md) | Pushs specific Profile from HrFlow and writes it to HrFlow_Profile__c & Co Custom Objects in Salesforce |
| [**Pull job list**](docs/pull_job_list.md) | Retrieves jobs from Salesforce HrFlow Job Custom Object and writes them to an Hrflow.ai board |


</p>

# 🐍 Quick Start Examples


To make sure you can successfully run the latest versions of the example scripts, you have to **install the package from PyPi**.
To browse the examples of actions corresponding to released versions of 🤗 this connector, you just need to import the module like this :


<p align="center">
<image src=https://github.com/Riminder/hrflow-connectors/assets/65894619/2ee70c18-e557-4b6b-8b2b-ea2243d58c1b width=90% height=100% >
</p>

Once the connector module is imported, you can leverage all the different actions that it offers. 

For more code details checkout connector code 

# 🔗 Useful Links

- 📄 Visit [Salesforce website](https://www.salesforce.com/en/) to learn more.
- ⚙️ Salesforce Developer portal: (https://developer.salesforce.com/)
- 💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/salesforce) on our Github.


# 👍 Special Thanks  
- 💻 HrFlow.ai : [the-forest-tree](https://github.com/the-forest-tree) - Software Engineer
- 🤝 Salesforce : [Salesforce Developer Centers](https://developer.salesforce.com/)