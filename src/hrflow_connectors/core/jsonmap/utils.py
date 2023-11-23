import typing as t
from dataclasses import dataclass
from enum import Enum, unique


@unique
class ErrorType(Enum):
    IllegalCharacter = "Illegal Character"
    InvalidSyntax = "Invalid Syntax"


class Error(t.NamedTuple):
    start: int
    end: int
    type: ErrorType
    details: str = ""


@dataclass
class JSONMapError(Exception):
    key: t.Optional[str]
    expression: str
    error: Error

    def as_string(self):
        result = (
            f"{self.error.type.value} for key {self.key} at col {self.error.start}:"
            f" {self.error.details}\n\n"
        )
        result += "{}\n".format(self.expression)
        result += " " * self.error.start + "^" * (self.error.end - self.error.start)
        return result
