import argparse
import json
import typing as t
from pathlib import Path

import lexer


def validate(source: dict[str, t.Any] | list[t.Any]) -> dict[str, t.Any] | list[t.Any]:
    if isinstance(source, dict):
        validated = dict()
        for key, value in source.items():
            validated[key] = validate(value)
        return validated
    if isinstance(source, list):
        validated = [validate(item) for item in source]
        return validated
    tokens, errors = lexer.make_tokens(source)
    return tokens


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("mapping", type=Path)

    p = parser.parse_args()
    if p.mapping.exists():
        mapping = json.loads(p.mapping.read_text())
        validated = validate(mapping)
        print(json.dumps(validated, indent=2))
