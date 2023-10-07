from enum import Enum


class CornerstoneOnDemandEnv(str, Enum):
    STAGE = "-stg"
    PILOT = "-pil"
    PRODUCTION = ""


class CornerstoneOnDemandPriority(str, Enum):
    CRITICAL_TO_FILL = "CriticalToFill"
    NORMAL = "Normal"
    NEUTRAL = "Neutral"


class CornerstoneOnDemandScope(str, Enum):
    JOB_REQUISITION_READ = "jobrequisition:read"
    JOB_APPLICANT_READ = "jobapplicant:read"
    JOB_APPLICATION_CREATE = "jobapplication:create"


class CornerstoneOnDemandStatus(str, Enum):
    DRAFT = "Draft"
    OPEN = "Open"
    CLOSED = "Closed"
    CANCELLED = "Cancelled"
    PENDING_APPROVAL = "Pending approval"
    APPROVAL_DENIED = "Approval Denied"
    OPEN_PENDING_REAPPROVAL = "Open - Pending Re - Approval"
    ON_HOLD = "On hold"


class CornerstoneOnDemandSupportedISOLanguageCode(str, Enum):
    CHINESE_HONG_KONG_SAR = "zh-HK"
    CHINESE_SIMPLIFIED_CHINA = "zh-CN"
    DUTCH_THE_NETHERLANDS = "nl-NL"
    ENGLISH_UNITED_STATES = "en-US"
    ENGLISH_UNITED_KINGDOM = "en-GB"
    FRENCH_CANADA = "fr-CA"
    FRENCH_FRANCE = "fr-FR"
    GERMAN_GERMANY = "de-DE"
    ITALIAN_ITALY = "it-IT"
    JAPANESE_JAPAN = "ja-JP"
    POLISH_POLAND = "pl-PL"
    PORTUGUESE_BRAZIL = "pt-BR"
    PORTUGUESE_PORTUGAL = "pt-PT"
    ROMANIAN_ROMANIA = "ro-RO"
    RUSSIAN_RUSSIA = "ru-RU"
    SPANISH_MEXICO = "es-MX"
    SPANISH_SPAIN = "es-ES"
    THAI_THAILAND = "th-TH"
    TURKISH_TURKEY = "tr-TR"


class CornerstoneOnDemandQuestionType(str, Enum):
    DISCLAIMER = "Disclaimer"
    COMPLIANCE = "Compliance"
    PRESCREENING = "Prescreening"


class CornerstoneOnDemandEndpoint(str, Enum):
    AUTHENTICATION = "oauth2/token"
    JOB_REQUISITION = "Recruiting/JobRequisitionDetails"
    JOB_APPLICANT = "Recruiting/JobApplicant"
    CANDIDATES = "x/candidate/v1/application"


class CornerstoneOnDemandGrantType(str, Enum):
    CLIENT_CREDENTIALS = "client_credentials"


class CornerstoneOnDemandCandidateType(str, Enum):
    INTERNAL = "Internal"
    EXTERNAL = "External"
