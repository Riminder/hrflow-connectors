import typing as t

from msgspec import Meta, Struct, field
from typing_extensions import Annotated


class Location(Struct):
    text: Annotated[t.Optional[str], Meta(description="Location text address.")] = None
    lat: Annotated[
        t.Optional[float], Meta(description="Geocentric latitude of the Location.")
    ] = None
    lng: Annotated[
        t.Optional[float], Meta(description="Geocentric longitude of the Location.")
    ] = None
    fields: Annotated[
        t.Optional[dict[str, t.Any]],
        Meta(description="other location attributes like country, country_code etc"),
    ] = None


class GeneralEntitySchema(Struct):
    name: Annotated[str, Meta(description="Identification name of the Object")]
    value: Annotated[
        t.Optional[str], Meta(description="Value associated to the Object's name")
    ] = None


class Skill(Struct):
    name: Annotated[str, Meta(description="Identification name of the skill")]
    type: Annotated[
        t.Literal["hard", "soft"], Meta(description="Type of the skill. hard or soft")
    ]
    value: Annotated[
        t.Optional[str], Meta(description="Value associated to the skill")
    ] = None


class Label(Struct):
    board_key: Annotated[
        str,
        Meta(description="Identification key of the Board containing the target Job."),
    ]
    job_key: Annotated[str, Meta(description="Identification key of the Job.")]
    job_reference: Annotated[str, Meta(description="Custom identifier of the Job.")]
    stage: Annotated[
        t.Literal["yes", "no", "later"],
        Meta(
            description=(
                "Stage associated to the Profile following the action of a recruiter"
                " (yes, no, later)."
            )
        ),
    ]
    date_stage: Annotated[
        str,
        Meta(description="Date of the stage edit action. type: ('datetime ISO 8601')"),
    ]
    rating: Annotated[
        t.Optional[t.Literal[1, 2, 3, 4, 5]],
        Meta(
            description=(
                "Rating associated to the Profile following the action of a recruiter"
                " (from 1 to 5)."
            )
        ),
    ]
    date_rating: Annotated[
        str, Meta(description="Date of the rating action. type: ('datetime ISO 8601')")
    ]


# Job
class Section(Struct):
    name: Annotated[
        t.Optional[str],
        Meta(
            description="Identification name of a Section of the Job. Example: culture",
        ),
    ] = None
    title: Annotated[
        t.Optional[str],
        Meta(description="Display Title of a Section. Example: Corporate Culture"),
    ] = None
    description: Annotated[
        t.Optional[str],
        Meta(description="Text description of a Section: Example: Our values areNone"),
    ] = None


class RangesFloat(Struct):
    name: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Identification name of a Range of floats attached "
                "to the Job. Example: salary"
            ),
        ),
    ] = None
    value_min: Annotated[
        t.Optional[float], Meta(description="Min value. Example: 500.")
    ] = None
    value_max: Annotated[
        t.Optional[float], Meta(description="Max value. Example: 100.")
    ] = None
    unit: Annotated[
        t.Optional[str], Meta(description="Unit of the value. Example: euros.")
    ] = None


class RangesDate(Struct):
    name: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "Identification name of a Range of dates attached"
                " to the Job. Example: availability."
            ),
        ),
    ] = None
    value_min: Annotated[
        t.Optional[str],
        Meta(description="Min value in datetime ISO 8601, Example: 500."),
    ] = None
    value_max: Annotated[
        t.Optional[str],
        Meta(description="Max value in datetime ISO 8601, Example: 1000"),
    ] = None


class HrFlowJob(Struct, kw_only=True):
    key: Annotated[
        t.Optional[str], Meta(description="Identification key of the Job.")
    ] = None
    reference: Annotated[
        t.Optional[str], Meta(description="Custom identifier of the Job.")
    ] = None
    name: Annotated[str, Meta(description="Job title.")]
    location: Annotated[Location, Meta(description="Job location object.")]
    sections: Annotated[list[Section], Meta(description="Job custom sections.")]
    url: Annotated[t.Optional[str], Meta(description="Job post original URL.")] = None
    summary: Annotated[
        t.Optional[str], Meta(description="Brief summary of the Job.")
    ] = None
    archieved_at: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "type: datetime ISO8601, Archive date of the Job. "
                "The value is null for unarchived Jobs."
            ),
        ),
    ] = None
    updated_at: Annotated[
        t.Optional[str],
        Meta(description="type: datetime ISO8601, Last update date of the Job."),
    ] = None
    created_at: Annotated[
        t.Optional[str],
        Meta(description="type: datetime ISO8601, Creation date of the Job."),
    ] = None
    skills: Annotated[
        t.Optional[list[Skill]], Meta(description="list of skills of the Job.")
    ] = None
    languages: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="list of spoken languages of the Job"),
    ] = None
    certifications: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="list of certifications of the Job."),
    ] = None
    courses: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="list of courses of the Job"),
    ] = None
    tasks: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="list of tasks of the Job"),
    ] = None
    tags: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="list of tags of the Job"),
    ] = None
    metadatas: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="list of metadatas of the Job"),
    ] = None
    ranges_float: Annotated[
        t.Optional[list[RangesFloat]], Meta(description="list of ranges of floats")
    ] = None
    ranges_date: Annotated[
        t.Optional[list[RangesDate]], Meta(description="list of ranges of dates")
    ] = None


# Profile
class InfoUrl(Struct):
    type: t.Literal["from_resume", "linkedin", "twitter", "facebook", "github"]
    url: t.Optional[str]


class ProfileInfo(Struct):
    full_name: t.Optional[str]
    first_name: t.Optional[str]
    last_name: t.Optional[str]
    email: t.Optional[str]
    phone: t.Optional[str]
    date_birth: Annotated[
        t.Optional[str], Meta(description="Profile date of birth")
    ] = None
    location: Annotated[
        t.Optional[Location], Meta(description="Profile location object")
    ] = None
    urls: Annotated[
        t.Optional[list[InfoUrl]],
        Meta(description="Profile social networks and URLs"),
    ] = None
    picture: Annotated[t.Optional[str], Meta(description="Profile picture url")] = None
    gender: Annotated[t.Optional[str], Meta(description="Profile gender")] = None
    summary: Annotated[t.Optional[str], Meta(description="Profile summary text")] = None


class Experience(Struct, kw_only=True):
    key: Annotated[
        t.Optional[str], Meta(description="Identification key of the Experience.")
    ] = None
    company: Annotated[
        t.Optional[str], Meta(description="Company name of the Experience.")
    ] = None
    logo: Annotated[t.Optional[str], Meta(description="Logo of the Company")] = None
    title: Annotated[t.Optional[str], Meta(description="Title of the Experience.")] = (
        None
    )
    description: Annotated[
        t.Optional[str], Meta(description="Description of the Experience.")
    ] = None
    location: Annotated[
        t.Optional[Location], Meta(description="Location object of the Experience.")
    ] = None
    date_start: Annotated[
        t.Optional[str],
        Meta(description="Start date of the experience. type: ('datetime ISO 8601')"),
    ] = None
    date_end: Annotated[
        t.Optional[str],
        Meta(description="End date of the experience. type: ('datetime ISO 8601')"),
    ] = None
    skills: Annotated[
        t.Optional[list[Skill]], Meta(description="List of skills of the Experience.")
    ] = None
    certifications: t.Optional[list[GeneralEntitySchema]]
    courses: t.Optional[list[GeneralEntitySchema]]
    tasks: t.Optional[list[GeneralEntitySchema]]


class Education(Struct, kw_only=True):
    key: Annotated[
        t.Optional[str], Meta(description="Identification key of the Education.")
    ] = None
    school: Annotated[
        t.Optional[str], Meta(description="School name of the Education.")
    ] = None
    logo: Annotated[t.Optional[str], Meta(description="Logo of the School")] = None
    title: Annotated[t.Optional[str], Meta(description="Title of the Education.")] = (
        None
    )
    description: Annotated[
        t.Optional[str], Meta(description="Description of the Education.")
    ] = None
    location: Annotated[
        t.Optional[Location], Meta(description="Location object of the Education.")
    ] = None
    date_start: Annotated[
        t.Optional[str],
        Meta(description="Start date of the Education. type: ('datetime ISO 8601')"),
    ] = None
    date_end: Annotated[
        t.Optional[str],
        Meta(description="End date of the Education. type: ('datetime ISO 8601')"),
    ] = None
    skills: Annotated[
        t.Optional[list[Skill]], Meta(description="List of skills of the Education.")
    ] = None
    certifications: t.Optional[list[GeneralEntitySchema]]
    courses: t.Optional[list[GeneralEntitySchema]]
    tasks: t.Optional[list[GeneralEntitySchema]]


class HrFlowProfile(Struct, kw_only=True):
    key: Annotated[
        t.Optional[str], Meta(description="Identification key of the Profile.")
    ] = None
    reference: Annotated[
        t.Optional[str], Meta(description="Custom identifier of the Profile.")
    ] = None
    info: Annotated[
        ProfileInfo, Meta(description="Object containing the Profile's info.")
    ]
    text_language: Annotated[
        str,
        Meta(description="Code language of the Profile. type: string code ISO 639-1"),
    ]
    text: Annotated[str, Meta(description="Full text of the Profile.")]
    archived_at: Annotated[
        t.Optional[str],
        Meta(
            description=(
                "type: datetime ISO8601, Archive date of the Profile."
                " The value is null for unarchived Profiles."
            ),
        ),
    ] = None
    updated_at: Annotated[
        t.Optional[str],
        Meta(description="type: datetime ISO8601, Last update date of the Profile."),
    ] = None
    created_at: Annotated[
        t.Optional[str],
        Meta(description="type: datetime ISO8601, Creation date of the Profile."),
    ] = None
    experiences_duration: Annotated[
        float, Meta(description="Total number of years of experience.")
    ]
    educations_duration: Annotated[
        float, Meta(description="Total number of years of education.")
    ]
    experiences: Annotated[
        t.Optional[list[Experience]],
        Meta(description="List of experiences of the Profile."),
    ] = field(default_factory=list)
    educations: Annotated[
        t.Optional[list[Education]],
        Meta(description="List of educations of the Profile."),
    ] = field(default_factory=list)
    attachments: Annotated[
        list, Meta(description="List of documents attached to the Profile.")
    ] = field(default_factory=list)
    skills: Annotated[
        t.Optional[list[Skill]], Meta(description="List of skills of the Profile.")
    ] = None
    languages: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="List of spoken languages of the profile"),
    ] = None
    certifications: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="List of certifications of the Profile."),
    ] = None
    courses: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="List of courses of the Profile."),
    ] = None
    tasks: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="List of tasks of the Profile."),
    ] = None
    interests: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="List of interests of the Profile."),
    ] = None
    tags: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="List of tags of the Profile."),
    ] = None
    metadatas: Annotated[
        t.Optional[list[GeneralEntitySchema]],
        Meta(description="List of metadatas of the Profile."),
    ] = None
    labels: Annotated[
        t.Optional[list[Label]], Meta(description="List of labels of the Profile.")
    ] = None


class ResumeToParse(Struct):
    raw: bytes
    content_type: str


class HrFlowProfileParsing(Struct):
    reference: Annotated[
        t.Optional[str], Meta(description="Custom identifier of the Profile.")
    ]
    created_at: Annotated[
        str, Meta(description="type: datetime ISO8601, Creation date of the Profile.")
    ]
    resume: ResumeToParse
    tags: Annotated[
        list[GeneralEntitySchema], Meta(description="List of tags of the Profile.")
    ]
    metadatas: Annotated[
        list[GeneralEntitySchema],
        Meta(description="List of metadatas of the Profile."),
    ]
