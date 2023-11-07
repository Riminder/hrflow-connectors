import enum
import logging
import os
import re
import subprocess
import typing as t
from contextvars import ContextVar
from datetime import datetime
from pathlib import Path

from pydantic import BaseModel
from pydantic.fields import ModelField

from hrflow_connectors.core import ActionName
from hrflow_connectors.core.connector import Connector
from hrflow_connectors.core.templates import Templates

logger = logging.getLogger(__name__)
CONNECTORS_DIRECTORY = Path(__file__).parent.parent / "connectors"

DONE_MARKUP = ":white_check_mark:"
KO_MARKUP = ":x:"


ROOT_README_TRACKED_ACTIONS = {
    ActionName.pull_job_list,
    ActionName.pull_profile_list,
    ActionName.push_profile,
    ActionName.push_job,
}


def CONNECTOR_LISTING_REGEXP_F(name: str) -> str:
    return (
        r"\|\s*\[?\*{0,2}(?i:(?P<name>"
        + r" ?".join([c for c in name if c.strip()])
        + r"))\*{0,2}(\]\([^)]+\))?\s*\|[^|]+\|[^|]+\|\s*(\*|_)(?P<release_date>[\d\/]+)(\*|_)\s*\|.+"
    )


ACTIONS_SECTIONS_REGEXP = (
    r"# 🔌 Connector Actions.+?\|\s*Action\s*\|\s*Description\s*\|.+?\|\s+?<\/p>"
)


GIT_UPDATE_TIMEOUT = 5
GIT_UPDATE_DATE = """
git ls-tree -r --name-only HEAD {base_connector_path}/{connector} | while read filename; do
  echo "$(git log -1 --format="%aI" -- $filename)"
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


def update_root_readme(connectors: t.List[Connector], root: Path) -> t.Dict:
    readme = root / "README.md"
    if readme.exists() is False:
        raise Exception("Failed to find root README.md at {}".format(readme))
    readme_content = readme.read_text()

    for connector in connectors:
        model = connector.model
        result = subprocess.run(
            GIT_UPDATE_DATE.format(
                connector=model.name.lower(),
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
        updated_at = datetime.fromisoformat(
            max(
                result.stdout.strip().splitlines(),
                key=lambda d: datetime.fromisoformat(d),
            )
        )
        actions_status = dict(
            zip(
                ROOT_README_TRACKED_ACTIONS,
                [KO_MARKUP] * len(ROOT_README_TRACKED_ACTIONS),
            )
        )
        for action in model.actions:
            if action.name in ROOT_README_TRACKED_ACTIONS:
                actions_status[action.name] = DONE_MARKUP

        pattern = CONNECTOR_LISTING_REGEXP_F(name=model.name)
        match = re.search(pattern, readme_content)
        if match is None:
            raise Exception(
                "Could not find listing for {} in root README with regexp={}".format(
                    model.name, pattern
                )
            )
        updated_listing = (
            "| [**{name}**]({readme_link}) | {type} | :white_check_mark: |"
            " *{release_date}* | *{updated_at}* | {pull_profile_list_status} |"
            " {pull_job_list_status} | {push_profile_status} | {push_job_status} |"
        ).format(
            name=match.group("name"),
            readme_link="./{base_connector_path}/{connector}/README.md".format(
                base_connector_path=BASE_CONNECTOR_PATH.get().strip("/"),
                connector=model.name.lower(),
            ),
            type=model.type.value,
            release_date=match.group("release_date"),
            updated_at=updated_at.strftime("%d/%m/%Y"),
            pull_profile_list_status=actions_status[ActionName.pull_profile_list],
            pull_job_list_status=actions_status[ActionName.pull_job_list],
            push_profile_status=actions_status[ActionName.push_profile],
            push_job_status=actions_status[ActionName.push_job],
        )
        readme_content = (
            readme_content[: match.start()]
            + updated_listing
            + readme_content[match.end() :]
        )
    readme.write_bytes(readme_content.encode())


def generate_docs(
    connectors: t.List[Connector], connectors_directory: Path = CONNECTORS_DIRECTORY
) -> None:
    update_root_readme(
        connectors=connectors, root=connectors_directory.parent.parent.parent
    )
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
        readme = connector_directory / "README.md"
        if readme.exists() is False:
            readme_content = Templates.get_template("connector_readme.md.j2").render(
                connector_name=model.name.capitalize(),
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
                    connector_name=model.name,
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
