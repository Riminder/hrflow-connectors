import typing as t
from dataclasses import dataclass, field
from hrflow_connectors.core.jsonmap.parser import (
    LiteralNode,
    DotAccessNode,
    ListAccessNode,
    EnhancedAccess,
    TokenType,
)


@dataclass
class Visitor:
    source_name: str = field(default="data")

    def literal(self, literal: LiteralNode):
        token = literal.token
        if token.value:
            if isinstance(token.value, str):
                return f'"{token.value}"'
            return token.value
        elif token.kind == TokenType.TRUE.name:
            return str(True)
        elif token.kind == TokenType.FALSE.name:
            return str(False)
        elif token.kind == TokenType.NULL.name:
            return str(None)

    def dot_access(self, dot_access: DotAccessNode):
        access = self.source_name
        path = dot_access.path
        if path == ".":
            return access
        currdot = path.find(".")
        while currdot >= 0 and currdot < len(path):
            nextdot = path.find(".", currdot + 1)
            begin = currdot + 1
            end = None if nextdot < 0 else nextdot - (path[nextdot - 1] == "?")
            name = path[begin:end]
            if currdot > 0 and path[currdot - 1] == "?":
                access += f'.get("{name}")'
            else:
                access += f'["{name}"]'
            currdot = nextdot
        return access

    def list_access(self, list_access: ListAccessNode):
        return list_access.node.accept(self) + f"[{list_access.index}]"

    def enhanced_access(self, enhanced_access: EnhancedAccess):
        if enhanced_access.enhancement == TokenType.FALSY:
            return (
                f"{enhanced_access.node.accept(self)} or"
                f" {enhanced_access.eventually.accept(self)}"
            )
        else:
            return ""


def translate(mappings: t.Dict[str, t.Any]):
    indent_str = "  "
    python_code = f"def format(data):\n{indent_str}return "

    def _translate_dict(data: t.Dict[str, t.Any], indent: str) -> str:
        if not data:
            return "{}"
        nonlocal indent_str
        key_name_indent = indent + indent_str
        result = "{\n"
        for name, value in data.items():
            if isinstance(value, dict):
                value = _translate_dict(value, key_name_indent)
            else:
                value = value.accept(Visitor())
            result += f'{key_name_indent}"{name}": {value},\n'
        result += indent + "}"
        return result

    python_code += _translate_dict(mappings, indent_str)
    python_code += "\n"
    return python_code
