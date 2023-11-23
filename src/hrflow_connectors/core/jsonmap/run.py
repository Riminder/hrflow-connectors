import argparse
import json
import typing as t
from pathlib import Path

import lexer
from utils import JSONMapError


def validate(
    source: t.Union[t.Dict[str, t.Any], t.List[t.Any]]
) -> t.Union[t.Dict[str, t.Any], t.List[t.Any]]:
    if isinstance(source, dict):
        validated = dict()
        for key, value in source.items():
            try:
                validated[key] = validate(value)
            except JSONMapError as e:
                if e.key is None:
                    e.key = "." + key
                else:
                    e.key = "." + key + e.key
                raise e
        return validated
    if isinstance(source, list):
        validated = []
        for i, item in enumerate(source):
            try:
                validated.append(validate(item))
            except JSONMapError as e:
                if e.key is None:
                    e.key = "[{}]".format(i)
                else:
                    e.key = "[{}]".format(i) + e.key
                raise e
        return validated
    tokens, error = lexer.make_tokens(source)
    if error:
        raise JSONMapError(key=None, expression=source, error=error)
    return tokens


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--mapping", type=Path)

    p = parser.parse_args()
    if p.mapping.exists():
        mapping = json.loads(p.mapping.read_text())
        try:
            validated = validate(mapping)
            print(json.dumps(validated, indent=2))
        except JSONMapError as e:
            print(e.as_string())
