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
FUNCTION_TOKENS = {
    TokenType.FLOAT_FN.name,
    TokenType.SPLIT_FN.name,
    TokenType.CONCAT_FN.name,
    TokenType.MAP_FN.name,
    TokenType.SUB_FN.name,
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
class IFNode:
    dot_access: DotAccessNode
    node: ASTNode

    def __repr__(self):
        return "IF {} THEN {}".format(self.dot_access, self.node)


@dataclass
class ListAccessNode:
    node: ASTNode
    index: int

    def __repr__(self):
        return "ListAccess[{}, {}]".format(self.node, self.index)


@dataclass
class EnhancedAccess(ASTNode):
    node: DotAccessNode | ListAccessNode
    eventually: ASTNode
    enhancement: t.Literal[TokenType.ACCESS_CATCH, TokenType.FALSY]

    def __repr__(self):
        enhancement = "falsy" if self.enhancement is TokenType.FALSY else "KeyError"
        if isinstance(self.node, DotAccessNode):
            return "EnhancedAccess[{} == IF {} => {}]".format(
                self.node.path, enhancement, self.eventually
            )
        return "EnhancedAccess[{} == IF {} => {}]".format(
            self.node, enhancement, self.eventually
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
        return repr({repr(key): repr(value) for key, value in self.items})


@dataclass
class FunctionNode(ASTNode):
    fn: TokenType
    args: list[ASTNode]

    def __repr__(self):
        if self.args:
            return (
                "${}[".format(self.fn.name)
                + ",".join([repr(arg) for arg in self.args])
                + "]"
            )
        return "${}".format(self.fn.name)


@dataclass
class PipedContextNode(ASTNode):
    parent_node: t.Union[EnhancedAccess, DotAccessNode]
    consumer: ASTNode


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
        return self.jsonmap()

    def jsonmap(self):
        res = ParseResult()
        token = self.current_token

        if token.kind == TokenType.IF.name:
            jsonmap = res.register(self.if_(self.expr))
        else:
            jsonmap = res.register(self.expr())

        if res.error:
            return res

        if self.current_token.kind == SpecialTokens.EOF.name:
            return res.success(jsonmap)
        return res.failure(
            Error(
                start=self.current_token.start,
                end=self.current_token.end,
                type=ErrorType.InvalidSyntax,
                details="Token not part of grammar yet {}".format(self.current_token),
            )
        )

    def if_(self, after_colon):
        res = ParseResult()
        token = self.current_token
        if token.kind != TokenType.IF.name:
            return res.failure(
                Error(
                    start=token.start,
                    end=token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting IF operator '{}' but found {}".format(
                        TokenType.IF.name, token
                    ),
                )
            )

        res.register(self.advance())
        dot_access = res.register(self.dot_access(simple=True))
        if res.error:
            return res

        if self.current_token.kind != TokenType.COLON.name:
            return res.failure(
                Error(
                    start=token.start,
                    end=token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Invalid IF block expecting '{}' but found {}".format(
                        TokenType.COLON.name, token
                    ),
                )
            )
        res.register(self.advance())

        expr = res.register(after_colon())
        if res.error:
            return res
        return res.success(IFNode(dot_access=dot_access, node=expr))

    def expr(self):
        res = ParseResult()
        token = self.current_token

        if token.kind == TokenType.L_BRAKET.name:
            res.register(self.advance())
            nodes = []
            if self.current_token.kind != TokenType.R_BRAKET.name:
                nodes.append(res.register(self.expr()))
                if res.error:
                    return res
                while self.current_token.kind == TokenType.COMMA.name:
                    res.register(self.advance())
                    nodes.append(res.register(self.expr()))
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
                if self.current_token.kind == TokenType.COLON.name:
                    res.register(self.advance())
                    value = res.register(self.expr())
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
                    if self.current_token.kind == TokenType.COLON.name:
                        res.register(self.advance())
                        value = res.register(self.expr())
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
            return res.success(MapNode(items))

        atom = res.register(self.atom())
        if res.error:
            return res
        return res.success(atom)

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
            if self.current_token.kind == TokenType.PASS_CONTEXT.name:
                res.register(self.advance())
                consumer = res.register(self.consumer())
                if res.error:
                    return res
                return res.success(
                    PipedContextNode(parent_node=dot_access, consumer=consumer)
                )
            return res.success(dot_access)

        if token.kind == TokenType.IF.name:
            if_atom = res.register(self.if_(self.atom))
            if res.error:
                return res
            return res.success(if_atom)

    def dot_access(self, simple: bool = False):
        res = ParseResult()
        token = self.current_token

        if token.kind != TokenType.DOT_ACCESS.name:
            return res.failure(
                Error(
                    start=token.start,
                    end=token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting Dot Access but found {}".format(token),
                )
            )

        node = DotAccessNode(token.value)

        res.register(self.advance())
        while self.current_token.kind in [
            TokenType.INDEX_ACCESS.name,
            TokenType.DOT_ACCESS.name,
        ]:
            if self.current_token.kind == TokenType.INDEX_ACCESS.name:
                node = ListAccessNode(node=node, index=self.current_token.value)
            else:
                node = DotAccessNode("{}{}".format(node, self.current_token.value))
            res.register(self.advance())

        if self.current_token.kind in [
            TokenType.ACCESS_CATCH.name,
            TokenType.FALSY.name,
        ]:
            enhancement = TokenType[self.current_token.kind]
            if simple is True:
                return res.failure(
                    Error(
                        start=self.current_token.start,
                        end=self.current_token.end,
                        type=ErrorType.InvalidSyntax,
                        details="Simple Dot Access expected but enhancement operator",
                    )
                )
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error:
                return res
            node = EnhancedAccess(
                node=node,
                enhancement=enhancement,
                eventually=expr,
            )
        return res.success(node)

    def float_fn(self):
        res = ParseResult()
        token = self.current_token

        if token.kind != TokenType.FLOAT_FN.name:
            return res.failure(
                Error(
                    start=token.start,
                    end=token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting $float function but found {}".format(token),
                )
            )
        res.register(self.advance())
        if self.current_token.kind == TokenType.L_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect call of function $float. No arguments are expected"
                    ),
                )
            )
        return res.success(FunctionNode(fn=TokenType.FLOAT_FN, args=[]))

    def split_fn(self):
        res = ParseResult()
        token = self.current_token

        if token.kind != TokenType.SPLIT_FN.name:
            return res.failure(
                Error(
                    start=token.start,
                    end=token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting $split function but found {}".format(token),
                )
            )
        res.register(self.advance())

        if self.current_token.kind != TokenType.L_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect function call. Expecting '(' but found {}".format(
                            self.current_token
                        )
                    ),
                )
            )
        res.register(self.advance())
        split_by = res.register(
            self.literal(
                only={
                    TokenType.RAW_STRING.name,
                    TokenType.QUOTED_RAW_STRING.name,
                }
            )
        )
        if res.error:
            return res
        if self.current_token.kind != TokenType.R_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect function call. Expecting ')' but found {}".format(
                            self.current_token
                        )
                    ),
                )
            )
        res.register(self.advance())
        return res.success(FunctionNode(fn=TokenType.SPLIT_FN, args=[split_by]))

    def sub_fn(self):
        res = ParseResult()

        if self.current_token.kind != TokenType.SUB_FN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting $sub function but found {}".format(
                        self.current_token
                    ),
                )
            )

        res.register(self.advance())

        if self.current_token.kind != TokenType.L_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect function call. Expecting '(' but found {}".format(
                            self.current_token
                        )
                    ),
                )
            )

        res.register(self.advance())

        pattern = res.register(
            self.literal(
                only={
                    TokenType.RAW_STRING.name,
                    TokenType.QUOTED_RAW_STRING.name,
                }
            )
        )

        if res.error:
            return res

        if self.current_token.kind != TokenType.COMMA.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Incorrect $sub call. Expecting ',' but found {}".format(
                        self.current_token
                    ),
                )
            )

        res.register(self.advance())

        replace_with = res.register(
            self.literal(
                only={
                    TokenType.RAW_STRING.name,
                    TokenType.QUOTED_RAW_STRING.name,
                }
            )
        )

        if res.error:
            return res

        if self.current_token.kind != TokenType.R_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Incorrect $sub call. Expecting ')' but found {}".format(
                        self.current_token
                    ),
                )
            )

        res.register(self.advance())

        return res.success(
            FunctionNode(fn=TokenType.SUB_FN, args=[pattern, replace_with])
        )

    def map_fn(self):
        res = ParseResult()
        token = self.current_token

        if token.kind != TokenType.MAP_FN.name:
            return res.failure(
                Error(
                    start=token.start,
                    end=token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting $map function but found {}".format(token),
                )
            )
        res.register(self.advance())
        if self.current_token.kind != TokenType.L_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect function call. Expecting '(' but found {}".format(
                            self.current_token
                        )
                    ),
                )
            )
        res.register(self.advance())
        expr = res.register(self.expr())
        if res.error:
            return res
        if self.current_token.kind != TokenType.R_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect function call. Expecting ')' but found {}".format(
                            self.current_token
                        )
                    ),
                )
            )
        res.register(self.advance())
        return res.success(FunctionNode(fn=TokenType.MAP_FN, args=[expr]))

    def concat_fn(self):
        res = ParseResult()
        token = self.current_token

        if token.kind != TokenType.CONCAT_FN.name:
            return res.failure(
                Error(
                    start=token.start,
                    end=token.end,
                    type=ErrorType.InvalidSyntax,
                    details="Expecting $concat function but found {}".format(token),
                )
            )
        res.register(self.advance())
        if self.current_token.kind != TokenType.L_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect function call. Expecting '(' but found {}".format(
                            self.current_token
                        )
                    ),
                )
            )
        res.register(self.advance())
        args = []
        if self.current_token.kind not in {
            TokenType.DOT_ACCESS.name,
            TokenType.RAW_STRING.name,
            TokenType.QUOTED_RAW_STRING.name,
        }:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect $concat call. Expecting literal"
                        " string or dot access but found {}".format(self.current_token)
                    ),
                )
            )
        if self.current_token.kind == TokenType.DOT_ACCESS.name:
            dot_access = res.register(self.dot_access())
            if res.error:
                return res
            args.append(dot_access)
        else:
            literal = res.register(
                self.literal(
                    only={
                        TokenType.RAW_STRING.name,
                        TokenType.QUOTED_RAW_STRING.name,
                    }
                )
            )
            if res.error:
                return res
            args.append(literal)

        while self.current_token.kind == TokenType.COMMA.name:
            res.register(self.advance())
            if self.current_token.kind not in {
                TokenType.DOT_ACCESS.name,
                TokenType.RAW_STRING.name,
                TokenType.QUOTED_RAW_STRING.name,
            }:
                return res.failure(
                    Error(
                        start=self.current_token.start,
                        end=self.current_token.end,
                        type=ErrorType.InvalidSyntax,
                        details=(
                            "Incorrect $concat call. Expecting literal"
                            " string or dot access but found {}".format(
                                self.current_token
                            )
                        ),
                    )
                )
            if self.current_token.kind == TokenType.DOT_ACCESS.name:
                dot_access = res.register(self.dot_access())
                if res.error:
                    return res
                args.append(dot_access)
            else:
                literal = res.register(
                    self.literal(
                        only={
                            TokenType.RAW_STRING.name,
                            TokenType.QUOTED_RAW_STRING.name,
                        }
                    )
                )
                if res.error:
                    return res
                args.append(literal)

        if self.current_token.kind != TokenType.R_PAREN.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details=(
                        "Incorrect function call. Expecting ')' but found {}".format(
                            self.current_token
                        )
                    ),
                )
            )
        res.register(self.advance())
        return res.success(FunctionNode(fn=TokenType.CONCAT_FN, args=args))

    def consumer(self):
        res = ParseResult()
        token = self.current_token

        for fn_name in FUNCTION_TOKENS:
            if token.kind == fn_name:
                call = getattr(self, fn_name.lower())
                fn = res.register(call())
                if res.error:
                    return res
                if self.current_token.kind == TokenType.PASS_CONTEXT.name:
                    if fn_name == TokenType.FLOAT_FN.name:
                        return res.failure(
                            Error(
                                start=self.current_token.start,
                                end=self.current_token.end,
                                type=ErrorType.InvalidSyntax,
                                details="PASS_CONTEXT not allowed after {}".format(fn),
                            )
                        )
                    else:
                        res.register(self.advance())
                        consumer = res.register(self.consumer())
                        if res.error:
                            return res
                        fn = PipedContextNode(parent_node=fn, consumer=consumer)
                return res.success(fn)

        expr = res.register(self.expr())
        if res.error:
            return res
        if self.current_token.kind == TokenType.PASS_CONTEXT.name:
            return res.failure(
                Error(
                    start=self.current_token.start,
                    end=self.current_token.end,
                    type=ErrorType.InvalidSyntax,
                    details="PASS_CONTEXT not allowed after {}".format(expr),
                )
            )
        return res.success(expr)
