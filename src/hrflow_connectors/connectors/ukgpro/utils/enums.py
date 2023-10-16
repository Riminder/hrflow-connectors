from enum import Enum


class UKGProGrantType(str, Enum):
    CLIENT_CREDENTIALS = "client_credentials"


class UKGProRecruitingScope(str, Enum):
    CANDIDATE_CREATE = "recruiting.domain.candidate-import.create"
    CANDIDATE_READ = "recruiting.domain.candidate-import.read"
    APPLICATION_CREATE = "recruiting.domain.application-import.create"
    APPLICATION_READ = "recruiting.domain.application-import.read"


class UKGProFileType(str, Enum):
    JPG = "image/jpeg"
    JPEG = "image/jpeg"
    PNG = "image/png"
    PDF = "application/pdf"
    DOC = "application/msword"
    DOCX = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"


class UKGProDocumentType(str, Enum):
    RESUME = "Resume"
    COVER_LETTER = "Cover Letter"
    PORTFOLIO = "Portfolio"
    OTHER = "Other"


class UKGProCreationMethod(str, Enum):
    CANDIDATE_IMPORT = "CandidateImport"
