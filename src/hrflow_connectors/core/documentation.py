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

from hrflow_connectors.core import ActionName
from hrflow_connectors.core.connector import Connector, get_import_name
from hrflow_connectors.core.templates import Templates

logger = logging.getLogger(__name__)
CONNECTORS_DIRECTORY = Path(__file__).parent.parent / "connectors"
ALL_TARGET_CONNECTORS_LIST_PATH = (
    Path(__file__).parent.parent / "data" / "connectors.json"
)
with open(ALL_TARGET_CONNECTORS_LIST_PATH, "r") as f:
    ALL_TARGET_CONNECTORS = json.load(f)


DONE_MARKUP = ":white_check_mark:"
KO_MARKUP = ":x:"
TARGET_MARKUP = ":dart:"
IN_PROGRESS_MARKUP = ":hourglass_flowing_sand:"


ROOT_README_TRACKED_ACTIONS = {
    ActionName.pull_job_list,
    ActionName.pull_profile_list,
    ActionName.push_profile,
    ActionName.push_job,
    ActionName.catch_profile,
}


ACTIONS_SECTIONS_REGEXP = (
    r"# 🔌 Connector Actions.+?\|\s*Action\s*\|\s*Description\s*\|.+?\|\s+?<\/p>"
)

GIT_UPDATE_EXCLUDE_PATTERN = r"(notebooks/\.gitkeep|mappings/format/\.gitkeep|README\.md|test\-config\.yaml|logo\.png|docs/)"
GIT_UPDATE_TIMEOUT = 5
GIT_UPDATE_DATE = """
git ls-tree -r --name-only HEAD {base_connector_path}/{connector} | while read filename; do
  echo "$(git log -1 --format="%cI" -- $filename) $filename"
done
"""

HRFLOW_CONNECTORS_REMOTE_URL = "https://github.com/Riminder/hrflow-connectors"
USE_REMOTE_REV: ContextVar[t.Optional[str]] = ContextVar("USE_REMOTE_REV", default=None)
BASE_CONNECTOR_PATH: ContextVar[t.Optional[str]] = ContextVar(
    "BASE_CONNECTOR_PATH", default="src/hrflow_connectors/connectors/"
)


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
                "object": connector_by_name.get(connector["name"]),
            }
            for connector in target_connectors
        ],
        key=lambda c: c["name"],
    )

    line_pattern = (
        "| **{name}** | {type} | {status} |"
        " {release_date} | {updated_at} | {pull_profile_list_status} |"
        " {pull_job_list_status} | {push_profile_status} | {push_job_status} |"
        " {catch_profile_status} |"
    )
    connectors_table = ""
    jobboards_table = ""
    for connector in all_connectors:
        actions_status = {
            action_name: "" for action_name in ROOT_README_TRACKED_ACTIONS
        }
        if connector["type"] in ["ATS", "HCM", "CRM"]:
            actions_status[ActionName.pull_job_list] = KO_MARKUP
            actions_status[ActionName.pull_profile_list] = KO_MARKUP
            actions_status[ActionName.push_profile] = KO_MARKUP
        elif connector["type"] == "Automation":
            actions_status[ActionName.catch_profile] = KO_MARKUP
        elif connector["type"] == "Job Board":
            actions_status[ActionName.pull_job_list] = KO_MARKUP
            actions_status[ActionName.push_job] = KO_MARKUP
            actions_status[ActionName.catch_profile] = KO_MARKUP

        if connector["object"] is None:
            updated_listing = line_pattern.format(
                name=connector["name"],
                type=connector["type"],
                status=(
                    IN_PROGRESS_MARKUP if connector["in_progress"] else TARGET_MARKUP
                ),
                release_date="",
                updated_at="",
                pull_profile_list_status=actions_status[ActionName.pull_profile_list],
                pull_job_list_status=actions_status[ActionName.pull_job_list],
                push_profile_status=actions_status[ActionName.push_profile],
                push_job_status=actions_status[ActionName.push_job],
                catch_profile_status=actions_status[ActionName.catch_profile],
            )
        else:
            model = connector["object"].model
            result = subprocess.run(
                GIT_UPDATE_DATE.format(
                    connector=model.subtype,
                    base_connector_path=BASE_CONNECTOR_PATH.get().rstrip("/"),
                ),
                shell=True,
                text=True,
                capture_output=True,
                timeout=GIT_UPDATE_TIMEOUT,
            )
            if result.stderr:
                raise Exception(
                    "Subprocess run for Git update dates failed for connector {} with"
                    " errors {}".format(model.name.lower(), result.stderr)
                )
            filtered = [
                line.split(" ")[0]
                for line in filter(
                    lambda line: not re.search(GIT_UPDATE_EXCLUDE_PATTERN, line),
                    result.stdout.strip().splitlines(),
                )
            ]
            updated_at = datetime.fromisoformat(
                max(
                    filtered,
                    key=lambda d: datetime.fromisoformat(d.replace("Z", "+00:00")),
                ).replace("Z", "+00:00")
            )

            for action in model.actions:
                if action.name in ROOT_README_TRACKED_ACTIONS:
                    actions_status[action.name] = DONE_MARKUP

            updated_listing = (
                "| [**{name}**]({readme_link}) | {type} | {status} |"
                " {release_date} | {updated_at} | {pull_profile_list_status} |"
                " {pull_job_list_status} | {push_profile_status} | {push_job_status} |"
                " {catch_profile_status} |"
            ).format(
                name=model.name,
                readme_link="./{base_connector_path}/{connector}/README.md".format(
                    base_connector_path=BASE_CONNECTOR_PATH.get().strip("/"),
                    connector=model.name.lower().replace(" ", ""),
                ),
                type=model.type.value,
                status=IN_PROGRESS_MARKUP if connector["in_progress"] else DONE_MARKUP,
                release_date=f'*{connector["release_date"]}*',
                updated_at=f'*{updated_at.strftime("%d/%m/%Y")}*',
                pull_profile_list_status=actions_status[ActionName.pull_profile_list],
                pull_job_list_status=actions_status[ActionName.pull_job_list],
                push_profile_status=actions_status[ActionName.push_profile],
                push_job_status=actions_status[ActionName.push_job],
                catch_profile_status=actions_status[ActionName.catch_profile],
            )
        if connector["type"] == "Job Board":
            jobboards_table += updated_listing + "\n"
        else:
            connectors_table += updated_listing + "\n"

    readme = root / "README.md"
    readme_content = root_template.render(
        connectors_table=connectors_table.strip("\n"),
        jobboards_table=jobboards_table.strip("\n"),
    )
    readme_content = py_37_38_compat_patch(readme_content)
    readme.write_bytes(readme_content.encode())


KEEP_EMPTY_FOLDER = ".gitkeep"


def generate_docs(
    connectors: t.List[Connector],
    target_connectors: t.List[t.Dict] = ALL_TARGET_CONNECTORS,
    connectors_directory: Path = CONNECTORS_DIRECTORY,
    root_template: Template = Templates.get_template("root_readme.md.j2"),
) -> None:
    update_root_readme(
        connectors=connectors,
        target_connectors=target_connectors,
        root=connectors_directory.parent.parent.parent,
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
