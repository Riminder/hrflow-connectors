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


@dataclass
class JSONMapError(Exception):
    key: t.Optional[str]
    expression: str
    error: Error
    details: str = ""

    def as_string(self):
        result = (
            f"{self.error.type.value} for key {self.key} at col {self.error.start}:"
            f" {self.details}\n\n"
        )
        result += self.expression + "\n"
        result += " " * self.error.start + "^" * (self.error.end - self.error.start)
        return result
