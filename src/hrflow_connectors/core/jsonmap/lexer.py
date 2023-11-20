import re
import typing as t
from enum import Enum, StrEnum, auto, unique


@unique
class TokenType(StrEnum):
    TRUE = r"true"
    FALSE = r"false"
    NULL = r"null"
    NUMBER = r"\d+(\.\d*)?"
    RAW_STRING = r"[^\.\?>\|$\s:{\['][^\s]*$"
    QUOTED_RAW_STRING = r"'[^']*'"
    DOT_ACCESS = r"(\??\.[a-zA-Z_\-09]*)+"
    ACCESS_CATCH = r"\|\|"
    FALSY = r">>"
    PASS_CONTEXT = r"\|"
    IF = r"\?\?"
    THEN = r":"
    FLOAT_FN = r"\$float"
    CONCAT_FN = r"\$concat\((?P<concat_args>[^)]*)\)"
    SPLIT_FN = r"\$split\((?P<split_args>[^)]*)\)"
    MAP_FN = r"\$map\((?P<map_args>[^)]*)\)"
    MAP = r"{[^}]*}"
    LIST = r"\[[^\]]*\]"


class Token(t.NamedTuple):
    kind: str
    value: t.Any = None


@unique
class SpecialTokens(Enum):
    SKIP = r"[ \t]+"
    MISMATCH = r"."


@unique
class ErrorType(StrEnum):
    IllegalCharacter = auto()


class Error(t.NamedTuple):
    type: ErrorType
    start: int
    end: int
    error: str


LiteralT = bool | int | float | None


def from_literal(literal: bool | int | float | None) -> list[Token]:
    if isinstance(literal, bool):
        if literal is True:
            return Token(TokenType.TRUE.name)
        return Token(TokenType.FALSE.name)
    if isinstance(literal, int) or isinstance(literal, float):
        return Token(TokenType.NUMBER.name, literal)
    return Token(TokenType.NULL.name)


def from_jsonmap(expression: str) -> tuple[list[Token], Error | None]:
    pattern = "|".join(
        f"(?P<{kind.name}>{kind.value})"
        for kind in list(TokenType) + list(SpecialTokens)
    )
    tokens = []
    for match in re.finditer(pattern, expression):
        kind = match.lastgroup
        value = match.group()
        if kind == TokenType.TRUE.name:
            value = True
        elif kind == TokenType.FALSE.name:
            value = False
        elif kind == TokenType.NULL.name:
            value = None
        elif kind == TokenType.NUMBER.name:
            value = float(value) if "." in value else int(value)
        elif kind == TokenType.QUOTED_RAW_STRING.name:
            value = value[1:-1]
        elif kind in [
            TokenType.SPLIT_FN.name,
            TokenType.CONCAT_FN.name,
            TokenType.MAP_FN.name,
        ]:
            call_group_name = kind.strip("_FN").lower() + "_args"
            value = match.group(call_group_name)
        elif kind == "SKIP":
            continue
        elif kind == "MISMATCH":
            return [], Error(
                type=ErrorType.IllegalCharacter,
                start=match.start(),
                end=match.end(),
                error=value,
            )
        tokens.append(Token(kind, value))
    return tokens, None


def make_tokens(value: LiteralT | str) -> tuple[list[Token], Error | None]:
    if isinstance(value, str):
        return from_jsonmap(value)
    return [from_literal(value)], None
