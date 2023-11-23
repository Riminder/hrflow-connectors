import typing as t
from dataclasses import dataclass, field

from hrflow_connectors.core.jsonmap.lexer import SpecialTokens, Token, TokenType
from hrflow_connectors.core.jsonmap.utils import Error, ErrorType

LITERAL_TOKENS = {
    TokenType.TRUE.name,
    TokenType.FALSE.name,
    TokenType.NULL.name,
    TokenType.NUMBER.name,
    TokenType.RAW_STRING.name,
    TokenType.QUOTED_RAW_STRING.name,
}
POST_DOT_ACCESS_TOKENS = {
    TokenType.ACCESS_CATCH.name,
    TokenType.FALSY.name,
    SpecialTokens.EOF.name,
}


@dataclass
class ASTNode:
    def __repr__(self):
        return "ASTNode"


@dataclass
class LiteralNode(ASTNode):
    token: Token

    def __repr__(self):
        return "{}:{}".format(self.token.kind, self.token.value)


@dataclass
class DotAccessNode(ASTNode):
    path: str

    def __repr__(self):
        return "DotAccess[{}]".format(self.path)


@dataclass
class EnhancedDotAccess(ASTNode):
    node: DotAccessNode
    eventually: ASTNode
    enhancement: t.Literal[TokenType.ACCESS_CATCH, TokenType.FALSY]

    def __repr__(self):
        enhancement = "falsy" if self.enhancement is TokenType.FALSY else "KeyError"
        return "DotAccess[{} == IF {} => {}]".format(
            self.node.path, enhancement, self.eventually
        )


@dataclass
class ParseResult:
    node: t.Optional[ASTNode] = None
    error: t.Optional[Error] = None

    def register(self, res: t.Any):
        if isinstance(res, ParseResult):
            if res.error:
                self.error = res.error
            return res.node
        return res

    def success(self, node: ASTNode):
        self.node = node
        return self

    def failure(self, error: Error):
        self.error = error
        return self


@dataclass
class Parser:
    tokens: t.List[Token]
    current_index: int = field(default=0, init=False)

    @property
    def current_token(self) -> Token:
        return self.tokens[self.current_index]

    def advance(self):
        if self.current_index < len(self.tokens):
            self.current_index += 1
        return self.current_token

    def parse(self):
        return self.expr()

    def expr(self):
        res = ParseResult()
        token = self.current_token

        if token.kind in LITERAL_TOKENS:
            res.register(self.advance())
            if self.current_token.kind == SpecialTokens.EOF.name:
                return res.success(LiteralNode(token))
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting EOF after Literal Token {} but found {}".format(
                        token, self.current_token
                    ),
                )
            )
        if token.kind == TokenType.DOT_ACCESS.name:
            dot_access = res.register(self.dot_access())
            if res.error:
                return res
            return res.success(dot_access)

        return res.failure(
            Error(
                start=token.start,
                end=token.end,
                type=ErrorType.InvalidSyntax,
                details="Token not part of grammar yet {}".format(token),
            )
        )

    def dot_access(self):
        res = ParseResult()
        token = self.current_token

        res.register(self.advance())
        if self.current_token.kind not in POST_DOT_ACCESS_TOKENS:
            return res.failure(
                Error(
                    start=token.start,
                    end=token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting one of {} after {} but found {}".format(
                        POST_DOT_ACCESS_TOKENS, token, self.current_token
                    ),
                )
            )

        next_token = self.current_token
        if next_token.kind == SpecialTokens.EOF.name:
            return res.success(DotAccessNode(token.value))

        if next_token.kind in [TokenType.ACCESS_CATCH.name, TokenType.FALSY.name]:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error:
                return res
            return res.success(
                EnhancedDotAccess(
                    node=DotAccessNode(token.value),
                    enhancement=TokenType[next_token.kind],
                    eventually=expr,
                )
            )
