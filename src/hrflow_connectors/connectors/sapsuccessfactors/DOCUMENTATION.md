# SAP Connector

## About SAP

Get software and technology solutions from SAP, the leader in business applications. Run simple with the best in cloud, analytics, mobile and IT solutions.

## Connector features

-   **Pull_Jobs:** Retrieves a list of job IDs from SAP API, retrieves the corresponding job information and location information for each ID, and sends the formatted data to an HrFlow board.
-   **Push_Profiles:** Retrieves candidate data from an HrFlow source, formats the data as a JSON object, and posts it to the SAP API to create new candidates.

## Import the connector
```from hrflow_connectors.connectors.sapsuccessfactors.connector import SAPSuccessFactors```

## Useful links

📄Visit [SAP](https://www.sap.com/) to learn more.

📄Check out SAP API's [documentation!](https://api.sap.com/)

💻 [Connector code](https://github.com/Riminder/hrflow-connectors/tree/master/src/hrflow_connectors/connectors/sapsuccessfactors) on our Github.

## Screenshots
<img width="1647" alt="Screen-Shot-2018-06-04-at-7 06 45-AM" src="https://user-images.githubusercontent.com/46778695/213191514-eb17b711-201c-4ab5-92cf-0542dfe966f0.png">
SAP candidate overview

## Data flow schema
![BreezyHR(7)](https://user-images.githubusercontent.com/46778695/213191595-10d81616-7190-42b5-a21c-da68baf68937.jpg)
