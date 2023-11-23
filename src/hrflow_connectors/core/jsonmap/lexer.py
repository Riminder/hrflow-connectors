import re
import typing as t
from enum import Enum, unique

from hrflow_connectors.core.jsonmap.utils import Error, ErrorType


@unique
class TokenType(Enum):
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
    start: int
    end: int
    value: t.Any = None


@unique
class SpecialTokens(Enum):
    SKIP = r"[ \t]+"
    MISMATCH = r"."
    EOF = "$"


LiteralT = t.Union[bool, int, float, None]


def from_literal(literal: LiteralT) -> Token:
    start = 0
    end = len(str(literal))
    if isinstance(literal, bool):
        if literal is True:
            return Token(TokenType.TRUE.name, start=start, end=end)
        return Token(TokenType.FALSE.name, start=start, end=end)
    if isinstance(literal, int) or isinstance(literal, float):
        return Token(TokenType.NUMBER.name, value=literal, start=start, end=end)
    return Token(TokenType.NULL.name, start=start, end=end)


def from_jsonmap(expression: str) -> t.Tuple[t.List[Token], t.Optional[Error]]:
    pattern = "|".join(
        f"(?P<{kind.name}>{kind.value})"
        for kind in list(TokenType) + list(SpecialTokens)
    )
    tokens = []
    for match in re.finditer(pattern, expression):
        kind = match.lastgroup
        value = match.group()
        if kind in [TokenType.TRUE.name, TokenType.FALSE.name, TokenType.NULL.name]:
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
        elif kind == SpecialTokens.SKIP.name:
            continue
        elif kind == SpecialTokens.EOF.name:
            value = None
        elif kind == SpecialTokens.MISMATCH.name:
            return [], Error(
                start=match.start(),
                end=match.end(),
                type=ErrorType.IllegalCharacter,
            )
        tokens.append(Token(kind, value=value, start=match.start(), end=match.end()))
    return tokens, None


def make_tokens(
    value: t.Union[LiteralT, str]
) -> t.Tuple[t.List[Token], t.Optional[Error]]:
    if isinstance(value, str):
        return from_jsonmap(value)
    return [
        from_literal(value),
        Token(SpecialTokens.EOF.name, start=len(str(value)), end=len(str(value))),
    ], None
