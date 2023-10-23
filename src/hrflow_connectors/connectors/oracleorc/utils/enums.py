from enum import Enum


class OracleORCLinkKind(str, Enum):
    COLLECTION = "collection"
    ITEM = "item"
    DESCRIBE = "describe"
    OTHER = "other"


class OracleORCLinkRelation(str, Enum):
    SELF = "self"
    LOV = "lov"
    PARENT = "parent"
    CANONICAL = "canonical"
    CHILD = "child"
    ENCLOSURE = "enclosure"
    ACTION = "action"
    CUSTOM = "custom"
