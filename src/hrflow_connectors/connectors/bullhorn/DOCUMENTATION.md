
# Bullhorn Connector

The Bullhorn-Hrflow connector allows you to synchronize data between the Bullhorn applicant tracking system (ATS) and the Hrflow platform.

  # About Bullhorn:
  
  **Bullhorn** is a cloud computing company headquartered in Boston, Massachusetts. The company provides customer relationship management (CRM), applicant tracking system (ATS) and operations software for the staffing industry. As of 2019, the company reported more than 11,000 customers in more than 150 countries. Besides its Boston headquarters, the company has operations in St. Louis, London, Brighton, Sydney and Rotterdam.

# Connector Features

**Push Profiles**

This action pushes a list of Hrflow profiles to Bullhorn. The profiles are first mapped to the correct format, and then pushed to Bullhorn in four steps:

1.  The profile itself is pushed to Bullhorn via the /entity/Candidate endpoint.
    
2.  The candidate's work history is enriched and added to the profile via the /entity/CandidateWorkHistory endpoint.
    
3.  The candidate's education is added to the profile via the /entity/CandidateEducation endpoint.
    
4.  Any attachments associated with the candidate, such as a CV or cover letter, are added to the profile via the /entity/CandidateFileAttachment endpoint.
    

**Read Jobs**

This action pulls job listings from Bullhorn via the /search/JobOrder endpoint and pushes them to an Hrflow job board. If there are more than 20 jobs in Bullhorn, additional Get requests will be made, changing the "start" parameter as needed.

**Pull Profiles**

This action consists of two separate actions:

1.  **The read_profiles** action pulls profiles from Bullhorn via the /entity/Candidate endpoint and pushes them to an Hrflow job board.
    
2.  The **read_profiles_parsing** action pulls a candidate's CV and uses Hrflow's parsing AI to enrich the previously pulled profile with information found on the CV.


# Useful links:

📄Visit [Bullhorn]([https://www.bullhorn.com/]) to learn more.

💻 [Connector code]([https://github.com/Sprenger07/hrflow-connectors/tree/feature/update-connector-workable/src/hrflow_connectors/connectors/workable](https://github.com/Sprenger07/hrflow-connectors/tree/feature/update-connector-workable/src/hrflow_connectors/connectors/workable)) on our Github.
