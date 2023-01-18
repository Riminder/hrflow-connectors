# TeamTailor Connector

## About Teamtailor

Teamtailor is the applicant tracking system made for all types of companies. With modern features optimized for you and your candidates, you will get everything you need to recruit successfully.

## Connector features

-   **Pull_Jobs:** Retrieves a list of job IDs from the TeamTailor API, retrieves the corresponding job information and location information for each ID, and sends the formatted data to an HrFlow board.
-   **Push_Profiles:** Retrieves candidate data from an HrFlow source, formats the data as a JSON object, and posts it to the TeamTailor API to create new candidates.

## Import the connector
```from hrflow_connectors.connectors.teamtailor.connector import Teamtailor```

## Useful links

📄Visit [Teamtailor](https://www.teamtailor.com/) to learn more.

📄Check out Teamtilor API's [documentation!](https://partner.teamtailor.com/)

💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/teamtailor) on our Github.

## Screenshots

![tealmtailor_candidat](https://user-images.githubusercontent.com/46778695/213188283-9066d469-b459-4f46-ae0e-5009f877706d.png)
Teamtailor candidate overview

## Data flow schema

![BreezyHR(6)](https://user-images.githubusercontent.com/46778695/213188767-7ddef358-0f0b-4d68-a14f-f0dcc42e4851.jpg)
