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
class ListNode(list):
    nodes: t.List[ASTNode]

    def __repr__(self):
        return repr(list(self.nodes))


@dataclass
class MapNode(ASTNode):
    items: t.List[t.Tuple[str, ASTNode]]

    def __repr__(self):
        return repr(dict(self.items))


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

        if token.kind == TokenType.L_BRAKET.name:
            res.register(self.advance())
            nodes = []
            if self.current_token.kind != TokenType.R_BRAKET.name:
                nodes.append(res.register(self.atom()))
                if res.error:
                    return res
                while self.current_token.kind == TokenType.COMMA.name:
                    res.register(self.advance())
                    nodes.append(res.register(self.atom()))
                    if res.error:
                        return res
            if self.current_token.kind != TokenType.R_BRAKET.name:
                return res.failure(
                    Error(
                        start=self.current_token.start,
                        end=self.current_token.end,
                        type=ErrorType.InvalidSyntax,
                        details="Expected ',' or ')' but found {}".format(
                            self.current_token
                        ),
                    )
                )
            res.register(self.advance())
            if self.current_token.kind != SpecialTokens.EOF.name:
                return res.failure(
                    Error(
                        start=self.current_token.start,
                        end=self.current_token.end,
                        type=ErrorType.InvalidSyntax,
                        details="Unexpected token after expression end {}".format(
                            self.current_token
                        ),
                    )
                )
            return res.success(ListNode(nodes))

        if token.kind == TokenType.L_CURLY.name:
            res.register(self.advance())
            items = []
            if self.current_token.kind != TokenType.R_CURLY.name:
                identifier = res.register(
                    self.literal(only={TokenType.RAW_STRING.name})
                )
                if res.error:
                    return res
                if self.current_token.kind == TokenType.COLLON.name:
                    res.register(self.advance())
                    value = res.register(self.atom())
                    if res.error:
                        return res
                    items.append((identifier, value))
                else:
                    return res.failure(
                        Error(
                            start=self.current_token.start,
                            end=self.current_token.end,
                            type=ErrorType.InvalidSyntax,
                            details="Expected ':' but found {}".format(
                                self.current_token
                            ),
                        )
                    )

                while self.current_token.kind == TokenType.COMMA.name:
                    res.register(self.advance())
                    identifier = res.register(
                        self.literal(only={TokenType.RAW_STRING.name})
                    )
                    if res.error:
                        return res
                    if self.current_token.kind == TokenType.COLLON.name:
                        res.register(self.advance())
                        value = res.register(self.atom())
                        if res.error:
                            return res
                        items.append((identifier, value))
                    else:
                        return res.failure(
                            Error(
                                start=self.current_token.start,
                                end=self.current_token.end,
                                type=ErrorType.InvalidSyntax,
                                details="Expected ':' but found {}".format(
                                    self.current_token
                                ),
                            )
                        )
            if self.current_token.kind != TokenType.R_CURLY.name:
                return res.failure(
                    Error(
                        start=self.current_token.start,
                        end=self.current_token.end,
                        type=ErrorType.InvalidSyntax,
                        details="Expected ',' or '}}' but found {}".format(
                            self.current_token
                        ),
                    )
                )
            res.register(self.advance())
            if self.current_token.kind != SpecialTokens.EOF.name:
                return res.failure(
                    Error(
                        start=self.current_token.start,
                        end=self.current_token.end,
                        type=ErrorType.InvalidSyntax,
                        details="Unexpected token after expression end {}".format(
                            self.current_token
                        ),
                    )
                )
            return res.success(MapNode(items))

        atom = res.register(self.atom())
        if res.error:
            return res
        if self.current_token.kind == SpecialTokens.EOF.name:
            return res.success(atom)

        return res.failure(
            Error(
                start=self.current_token.start,
                end=self.current_token.end,
                type=ErrorType.InvalidSyntax,
                details="Token not part of grammar yet {}".format(self.current_token),
            )
        )

    def literal(self, only: t.Set[str] = None):
        only = only or LITERAL_TOKENS

        res = ParseResult()
        token = self.current_token
        if token.kind in only:
            res.register(self.advance())
            return res.success(LiteralNode(token))
        return res.failure(
            Error(
                start=self.current_token.start,
                end=self.current_token.end,
                type=ErrorType.InvalidSyntax,
                details="Expecting literal value of type {} but found {}".format(
                    only, self.current_token
                ),
            )
        )

    def atom(self):
        res = ParseResult()
        token = self.current_token

        if token.kind in LITERAL_TOKENS:
            return self.literal()

        if token.kind == TokenType.DOT_ACCESS.name:
            dot_access = res.register(self.dot_access())
            if res.error:
                return res
            return res.success(dot_access)

    def dot_access(self):
        res = ParseResult()
        token = self.current_token

        res.register(self.advance())
        next = self.current_token
        if next.kind in [
            TokenType.ACCESS_CATCH.name,
            TokenType.FALSY.name,
        ]:
            res.register(self.advance())
            expr = res.register(self.atom())
            if res.error:
                return res
            return res.success(
                EnhancedDotAccess(
                    node=DotAccessNode(token.value),
                    enhancement=TokenType[next.kind],
                    eventually=expr,
                )
            )
