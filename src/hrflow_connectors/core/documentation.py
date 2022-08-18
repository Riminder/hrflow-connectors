import enum
import logging
import os
import typing as t
from pathlib import Path

from pydantic import BaseModel
from pydantic.fields import ModelField

from hrflow_connectors.core.connector import Connector
from hrflow_connectors.core.templates import (
    ACTION_DOCUMENTATION_TEMAPLTE,
    CONNECTOR_README_TEMPLATE,
)

logger = logging.getLogger(__name__)
CONNECTORS_DIRECTORY = Path(__file__).parent.parent / "connectors"


class TemplateField(BaseModel):
    name: str
    type: str
    required: bool
    description: str
    example: str
    default: str


def field_example(field: ModelField) -> str:
    if callable(field.default):
        return "lambda *args, **kwargs: None # Put your code logic here"

    if field.default is not None:
        return str(field.default)

    if field.default_factory is not None:
        return str(field.default_factory())

    field_type = field.outer_type_
    if isinstance(field_type, enum.EnumMeta):
        return '"{}"'.format(list(field_type)[0].value)

    if field_type is str:
        return '"your_{}"'.format(field.name)

    if field_type in [int, float, bool]:
        return str(field_type())

    return "***"


def field_default(field: ModelField, documentation_path: Path) -> str:
    if callable(field.default):
        relative_filepath = os.path.relpath(
            field.default.__code__.co_filename, documentation_path
        )
        return "[`{}`]({}#L{})".format(
            field.default.__code__.co_name,
            relative_filepath,
            field.default.__code__.co_firstlineno,
        )

    if field.default_factory is not None:
        return str(field.default_factory())

    return str(field.default)


def field_type(field: ModelField) -> str:
    if isinstance(field.outer_type_, enum.EnumMeta):
        return "str"
    if field.type_.__name__ == "Callable":
        return str(field.outer_type_)
    return field.type_.__name__


def get_template_fields(
    fields: t.List[ModelField], documentation_path: Path
) -> t.List[TemplateField]:
    return [
        TemplateField(
            name=field.name,
            type=field_type(field),
            required=field.required,
            description=field.field_info.description or "",
            example=field_example(field),
            default=field_default(field, documentation_path),
        )
        for field in fields
        if not field.field_info.const
        and field.field_info.extra.get("skip_from_docs", False) is False
    ]


def generate_docs(
    connectors: t.List[Connector], connectors_directory: Path = CONNECTORS_DIRECTORY
) -> None:
    for connector in connectors:
        model = connector.model
        connector_directory = connectors_directory / model.name.lower()
        if not connector_directory.is_dir():
            logging.error(
                "Skipping documentation for {}: no directory found at {}".format(
                    model.name, connector_directory
                )
            )
            continue
        readme_content = CONNECTOR_README_TEMPLATE.render(
            connector_name=model.name.capitalize(),
            description=model.description,
            url=model.url,
            actions=model.actions,
        )
        readme = connector_directory / "README.md"
        readme.write_bytes(readme_content.encode())
        if len(model.actions) > 0:
            action_docs_directory = connector_directory / "docs"
            if not action_docs_directory.is_dir():
                action_docs_directory.mkdir()
            for action in model.actions:
                action_fields = get_template_fields(
                    fields=action.parameters.__fields__.values(),
                    documentation_path=action_docs_directory,
                )
                origin_fields = get_template_fields(
                    fields=action.origin.read.parameters.__fields__.values(),
                    documentation_path=action_docs_directory,
                )
                target_fields = get_template_fields(
                    fields=action.target.write.parameters.__fields__.values(),
                    documentation_path=action_docs_directory,
                )
                action_documentation_content = ACTION_DOCUMENTATION_TEMAPLTE.render(
                    connector_name=model.name,
                    action_name=action.name,
                    description=action.description,
                    action_fields=action_fields,
                    origin_name=action.origin.name,
                    origin_fields=origin_fields,
                    origin_endpoints=action.origin.read.endpoints,
                    target_name=action.target.name,
                    target_fields=target_fields,
                    target_endpoints=action.target.write.endpoints,
                )
                action_documentation = action_docs_directory / "{}.md".format(
                    action.name
                )
                action_documentation.write_bytes(action_documentation_content.encode())
