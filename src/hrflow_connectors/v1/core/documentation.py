import enum
import json
import logging
import os
import re
import subprocess
import typing as t
from contextvars import ContextVar
from datetime import datetime
from pathlib import Path

from jinja2 import Template
from pydantic import BaseModel
from pydantic.fields import ModelField
from typing_extensions import TypeGuard

from hrflow_connectors.v1.core.common import ALL_TARGET_CONNECTORS_LIST_PATH
from hrflow_connectors.v1.core.connector import (
    MAIN_IMPORT_NAME,
    Connector,
    get_import_name,
)
from hrflow_connectors.v1.core.templates import Templates
from hrflow_connectors.v2 import __CONNECTORS__ as __CONNECTORS__V2
from hrflow_connectors.v2.core.connector import Connector as V2Connector

logger = logging.getLogger(__name__)
CONNECTORS_DIRECTORY = Path(__file__).parent.parent / "connectors"

ACTIONS_SECTIONS_REGEXP = (
    r"# 🔌 Connector Actions.+?\|\s*Action\s*\|\s*Description\s*\|.+?\|\s+?<\/p>"
)

GIT_UPDATE_EXCLUDE_PATTERN = r"(notebooks/\.gitkeep|mappings/format/\.gitkeep|README\.md|test\-config\.yaml|logo\.png|docs/)"
GIT_UPDATE_TIMEOUT = 5
# The git log command was updated with the
# of following arguments to discard the commits
# which are related to v2 migration
# --grep "[v1-v2-migration]" --invert-grep
# In case no commit pass the condition below then
# then "pre_v2_updated_at" from connectors.json is used
GIT_UPDATE_DATE = """
git ls-tree -r --name-only HEAD {base_connector_path}/{connector} | while read filename; do
  echo "$(git log -1 --grep "[v1-v2-migration]" --invert-grep --format="%cI" -- $filename) $filename"
done
"""

HRFLOW_CONNECTORS_REMOTE_URL = "https://github.com/Riminder/hrflow-connectors"
USE_REMOTE_REV: ContextVar[t.Optional[str]] = ContextVar("USE_REMOTE_REV", default=None)
BASE_CONNECTOR_PATH: ContextVar[t.Optional[str]] = ContextVar(
    "BASE_CONNECTOR_PATH", default="src/hrflow_connectors/v1/connectors/"
)
PREMIUM_STATUS = ":lock: Premium"
PREMIUM_README_LINK = "https://forms.gle/pokoE9pAjSVSFtCe7"
OPENSOURCE_STATUS = ":book: Open source"

V2_CONNECTORS_BY_NAME = {connector.name: connector for connector in __CONNECTORS__V2}


class InvalidConnectorReadmeFormat(Exception):
    pass


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
        if isinstance(field.default, str):
            return '"{}"'.format(field.default)
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
        filepath = os.path.relpath(
            field.default.__code__.co_filename, documentation_path
        )
        if (
            "site-packages/hrflow_connectors/" in filepath
            and USE_REMOTE_REV.get() is not None
        ):
            filepath = "{}/tree/{}/src/hrflow_connectors/{}".format(
                HRFLOW_CONNECTORS_REMOTE_URL,
                USE_REMOTE_REV.get(),
                filepath.split("/hrflow_connectors/")[-1],
            )
        return "[`{}`]({}#L{})".format(
            field.default.__code__.co_name,
            filepath,
            field.default.__code__.co_firstlineno,
        )

    if field.default_factory is not None:
        return str(field.default_factory())

    return str(field.default)


def field_type(field: ModelField) -> str:
    if field.outer_type_ in [int, float, str, bool]:
        return field.outer_type_.__name__
    if isinstance(field.outer_type_, enum.EnumMeta):
        return "str"
    return str(field.outer_type_)


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


def py_37_38_compat_patch(content: str) -> str:
    """
    The way t.Optional[T] is stringified is different accross supported python versions:
        - Python 3.7, 3.8 --> typing.Union[T, NoneType]
        - Python >= 3.9 --> t.Optional[T]
    This creates inconsistency when generating the doc with accross these versions.
    This function changes any older string versions to match with >=3.9
    """
    return re.sub(
        r"Union\[([\w\.]+), NoneType\]",
        lambda match: f"Optional[{match.group(1)}]",
        content,
    )


def ensure_gitkeep(directory: Path, gitkeep_filename: str = ".gitkeep") -> None:
    gitkeep_file = directory / gitkeep_filename
    create_empty_file = True

    if directory.is_dir():
        for child in directory.iterdir():
            if not child.name == gitkeep_file.name:
                create_empty_file = False
                try:
                    gitkeep_file.unlink()
                except FileNotFoundError:
                    pass
                break
    else:
        directory.mkdir(parents=True)

    if create_empty_file:
        gitkeep_file.touch()


def connector_is_v2(
    connector: t.Union[Connector, V2Connector],
) -> TypeGuard[V2Connector]:
    return isinstance(connector, V2Connector)


def update_root_readme(
    connectors: t.List[Connector],
    target_connectors: t.List[t.Dict],
    root: Path,
    root_template: Template,
) -> t.Dict:
    connector_by_name = {connector.model.name: connector for connector in connectors}
    all_connectors = sorted(
        [
            {
                **connector,
                "object": V2_CONNECTORS_BY_NAME.get(
                    connector["name"], connector_by_name.get(connector["name"])
                ),
            }
            for connector in target_connectors
        ],
        key=lambda c: c["name"].lower(),
    )

    line_pattern = (
        "| [**{name}**]({readme_link}) | {type} | {status} |"
        " {release_date} | {updated_at} |"
    )
    opensource_connectors_table = ""
    opensource_jobboards_table = ""
    premium_connectors_table = ""
    premium_jobboards_table = ""
    for connector in all_connectors:
        if connector["object"] is None:
            updated_listing = line_pattern.format(
                name=connector["name"],
                readme_link=PREMIUM_README_LINK,
                type=connector["type"],
                status=PREMIUM_STATUS,
                release_date="",
                updated_at="",
            )
            if connector["type"] == "Job Board":
                premium_jobboards_table += updated_listing + "\n"
            else:
                premium_connectors_table += updated_listing + "\n"
        else:
            connector_object = t.cast(
                t.Union[Connector, V2Connector], connector["object"]
            )
            if connector_is_v2(connector_object):
                name = connector_object.name
                subtype = connector_object.subtype
                connector_type = connector_object.type
                base_connector_path = (
                    BASE_CONNECTOR_PATH.get().rstrip("/").replace("v1", "v2")
                )
            else:
                name = connector_object.model.name
                subtype = connector_object.model.subtype
                connector_type = connector_object.model.type
                base_connector_path = BASE_CONNECTOR_PATH.get().rstrip("/")

            result = subprocess.run(
                GIT_UPDATE_DATE.format(
                    connector=subtype,
                    base_connector_path=base_connector_path,
                ),
                shell=True,
                text=True,
                capture_output=True,
                timeout=GIT_UPDATE_TIMEOUT,
            )
            if result.stderr:
                raise Exception(
                    "Subprocess run for Git update dates failed for connector {} with"
                    " errors {}".format(subtype, result.stderr)
                )
            filtered = [
                line.split(" ")[0]
                for line in filter(
                    lambda line: not re.search(GIT_UPDATE_EXCLUDE_PATTERN, line),
                    result.stdout.strip().splitlines(),
                )
            ]
            non_empty = [entry for entry in filtered if entry != ""]
            if len(non_empty) == 0:
                updated_at = connector["pre_v2_updated_at"]
            else:
                updated_at = datetime.fromisoformat(
                    max(
                        non_empty,
                        key=lambda d: datetime.fromisoformat(d.replace("Z", "+00:00")),
                    ).replace("Z", "+00:00")
                ).strftime("%d/%m/%Y")

            updated_listing = line_pattern.format(
                name=name,
                readme_link="./{base_connector_path}/{connector}/README.md".format(
                    base_connector_path=base_connector_path,
                    connector=subtype,
                ),
                type=connector_type.value,
                status=OPENSOURCE_STATUS,
                release_date=f'*{connector["release_date"]}*',
                updated_at=f"*{updated_at}*",
            )

            if connector["type"] == "Job Board":
                opensource_jobboards_table += updated_listing + "\n"
            else:
                opensource_connectors_table += updated_listing + "\n"

    readme = root / "README.md"
    readme_content = root_template.render(
        opensource_connectors_table=opensource_connectors_table.strip("\n"),
        opensource_jobboards_table=opensource_jobboards_table.strip("\n"),
        premium_connectors_table=premium_connectors_table.strip("\n"),
        premium_jobboards_table=premium_jobboards_table.strip("\n"),
    )
    readme_content = py_37_38_compat_patch(readme_content)
    readme.write_bytes(readme_content.encode())


KEEP_EMPTY_FOLDER = ".gitkeep"


def generate_docs(
    connectors: t.List[Connector],
    target_connectors: t.Optional[t.List[t.Dict]] = None,
    connectors_directory: Path = CONNECTORS_DIRECTORY,
    root_template: Template = Templates.get_template("root_readme.md.j2"),
) -> None:
    if target_connectors is None:
        with open(ALL_TARGET_CONNECTORS_LIST_PATH, "r") as f:
            target_connectors = json.load(f)
    update_root_readme(
        connectors=connectors,
        target_connectors=target_connectors,
        root=connectors_directory.parent.parent.parent.parent,
        root_template=root_template,
    )
    for connector in connectors:
        model = connector.model
        connector_directory = connectors_directory / model.subtype
        if not connector_directory.is_dir():
            logging.error(
                "Skipping documentation for {}: no directory found at {}".format(
                    model.name, connector_directory
                )
            )
            continue

        import_name = get_import_name(connector)

        readme = connector_directory / "README.md"
        if readme.exists() is False:
            readme_content = Templates.get_template("connector_readme.md.j2").render(
                connector_name=model.name.replace(" ", "").capitalize(),
                description=model.description,
                url=model.url,
                actions=model.actions,
            )
            readme_content = py_37_38_compat_patch(readme_content)
            readme.write_bytes(readme_content.encode())
        else:
            readme_content = readme.read_text()
            match = re.search(ACTIONS_SECTIONS_REGEXP, readme_content, re.DOTALL)
            if match is None:
                raise InvalidConnectorReadmeFormat(
                    "README.md for connector {} does not respect standard format. No"
                    " actions section found".format(model.name)
                )
            updated_actions_content = Templates.get_template(
                "connector_actions.md.j2"
            ).render(
                actions=model.actions,
            )
            updated_readme_content = "{before}{actions}{after}".format(
                before=readme_content[: match.start()],
                actions=updated_actions_content,
                after=readme_content[match.end() :],
            )
            updated_readme_content = py_37_38_compat_patch(updated_readme_content)
            readme.write_bytes(updated_readme_content.encode())

        notebooks_directory = connector_directory / "notebooks"
        ensure_gitkeep(notebooks_directory, KEEP_EMPTY_FOLDER)

        format_mappings_directory = connector_directory / "mappings" / "format"
        ensure_gitkeep(format_mappings_directory, KEEP_EMPTY_FOLDER)

        if len(model.actions) > 0:
            action_docs_directory = connector_directory / "docs"
            if not action_docs_directory.is_dir():
                action_docs_directory.mkdir()
            for action in model.actions:
                action_name = action.name.value
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
                action_documentation_content = Templates.get_template(
                    "action_readme.md.j2"
                ).render(
                    main_module=MAIN_IMPORT_NAME.get(),
                    import_name=import_name,
                    action_name=action_name,
                    description=action.description,
                    action_fields=action_fields,
                    origin_name=action.origin.name,
                    origin_fields=origin_fields,
                    origin_endpoints=action.origin.read.endpoints,
                    target_name=action.target.name,
                    target_fields=target_fields,
                    target_endpoints=action.target.write.endpoints,
                )
                action_documentation_content = py_37_38_compat_patch(
                    action_documentation_content
                )
                action_documentation = action_docs_directory / "{}.md".format(
                    action_name
                )
                action_documentation.write_bytes(action_documentation_content.encode())
