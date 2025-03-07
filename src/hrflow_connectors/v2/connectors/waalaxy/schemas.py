from msgspec import Struct


class WaalaxyProfile(Struct):
    _id: str
    firstName: str
    lastName: str
    occupation: str
    location: str
    proEmail: str
    phoneNumbers: str
    prospectList: str
    linkedinUrl: str
    premium: str
    jobSeeker: str
    profileStatus: str
    messageSent: str
    messageReplied: str
    emailSent: str
    emailReplied: str
    connectedAt: str
    firstMessageAt: str
    tags: str
    company_name: str
    company_linkedinUrl: str
    company_website: str
    job_title: str
    linkedinEmail: str
    salesNavigatorId: str
    profileId: str
