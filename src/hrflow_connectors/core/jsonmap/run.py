import argparse
import json
import typing as t
from pathlib import Path

from hrflow_connectors.core.jsonmap import lexer, parser
from hrflow_connectors.core.jsonmap.utils import JSONMapError


def json_encodable_ast(node: parser.ASTNode) -> t.Any:
    if isinstance(node, parser.ListNode):
        return [json_encodable_ast(node) for node in node.nodes]
    if isinstance(node, parser.MapNode):
        return {key.token.value: json_encodable_ast(value) for key, value in node.items}
    if isinstance(node, parser.PipedContextNode):
        context = json_encodable_ast(node.parent_node)
        if (
            isinstance(node.consumer, parser.FunctionNode)
            and node.consumer.fn is lexer.TokenType.MAP_FN
        ):
            return [
                "For each item in {}[]".format(context),
                json_encodable_ast(node.consumer.args[0]),
            ]
        consumer = json_encodable_ast(node.consumer)
        if isinstance(consumer, list):
            return [
                "{} | {}".format(context, json_encodable_ast(item)) for item in consumer
            ]
        if isinstance(consumer, dict):
            return {
                key: "{} | {}".format(context, json_encodable_ast(value))
                for key, value in consumer.items()
            }
        return "{} | {}".format(context, consumer)
    return repr(node)


def ast(
    source: t.Union[t.Dict[str, t.Any], t.List[t.Any]]
) -> t.Union[t.Dict[str, t.Any], t.List[t.Any]]:
    if isinstance(source, dict):
        parsed = dict()
        for key, value in source.items():
            try:
                parsed[key] = ast(value)
            except JSONMapError as e:
                if e.key is None:
                    e.key = "." + key
                else:
                    e.key = "." + key + e.key
                raise e
        return parsed
    if isinstance(source, list):
        parsed = []
        for i, item in enumerate(source):
            try:
                parsed.append(ast(item))
            except JSONMapError as e:
                if e.key is None:
                    e.key = "[{}]".format(i)
                else:
                    e.key = "[{}]".format(i) + e.key
                raise e
        return parsed

    tokens, error = lexer.make_tokens(source)
    if error:
        raise JSONMapError(key=None, expression=source, error=error)

    parse_result = parser.Parser(tokens).parse()
    if parse_result.error:
        raise JSONMapError(key=None, expression=source, error=parse_result.error)
    return json_encodable_ast(parse_result.node)


if __name__ == "__main__":
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("--mapping", type=Path)

    p = arg_parser.parse_args()
    if p.mapping.exists():
        mapping = json.loads(p.mapping.read_text())
        try:
            mapping_ast = ast(mapping)
            print(json.dumps(mapping_ast, indent=2))
        except JSONMapError as e:
            print(e.as_string())
