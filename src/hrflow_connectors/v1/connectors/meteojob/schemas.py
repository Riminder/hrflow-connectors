import typing as t

from pydantic import BaseModel


class MeteojobEventObject(BaseModel):
    type: str
    jobkey: t.Optional[str]
    firstName: t.Optional[str]
    lastName: t.Optional[str]
    phone: t.Optional[str]
    email: str
    cvBase64: str
    coverText: t.Optional[str]
    profilecountry: t.Optional[str]
    profileregions: t.Optional[str]
    profiledomains: t.Optional[str]
    joblien_annonce_site_carriere: t.Optional[str]
    statisticsource: t.Optional[str]
    statisticjbsource: t.Optional[str]
