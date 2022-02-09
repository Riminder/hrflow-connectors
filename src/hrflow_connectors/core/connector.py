import enum
import json
import logging
import typing as t
import uuid
from functools import partial

from pydantic import BaseModel, Field, ValidationError, create_model

from hrflow_connectors.core.templates import WORKFLOW_TEMPLATE
from hrflow_connectors.core.warehouse import Warehouse

logger = logging.getLogger(__name__)


class ConnectorActionAdapter(logging.LoggerAdapter):
    def process(self, msg: str, kwargs: t.Dict) -> t.Tuple[str, t.Dict]:
        tags = [
            "[{}={}]".format(tag["name"], tag["value"])
            for tag in self.extra["log_tags"]
        ]
        return (
            "{}: {}".format(
                "".join(tags),
                msg,
            ),
            kwargs,
        )


class ActionStatus(enum.Enum):
    success = enum.auto()
    failure = enum.auto()


LogicFunctionType = t.Callable[[t.Dict], t.Union[t.Dict, None]]
LogicsTemplate = """
import typing as t

def logic_1(item: t.Dict) -> t.Union[t.Dict, None]:
    return None

def logic_2(item: t.Dict) -> t.Uniont[t.Dict, None]:
    return None

logics = [logic_1, logic_2]
"""
FormatFunctionType = t.Callable[[t.Dict], t.Dict]
FormatTemplate = """
import typing as t

def format(item: t.Dict) -> t.Dict:
    return item
"""
FormatDescription = "Formatting function"


class BaseActionParameters(BaseModel):
    logics: t.List[LogicFunctionType] = Field(
        default_factory=list, description="List of logic functions"
    )
    format: FormatFunctionType = Field(lambda x: x, description=FormatDescription)

    class Config:
        @staticmethod
        def schema_extra(
            schema: t.Dict[str, t.Any], model: t.Type["BaseActionParameters"]
        ) -> None:
            # JSON has no equivalent for Callable type which is used for
            # logics and format. Thus we hardcode properties for both here
            schema["properties"]["logics"] = (
                {
                    "title": "logics",
                    "description": (
                        "List of logic functions. Each function should have"
                        " the following signature {}. The final list should be exposed "
                        "in a variable named 'logics'.".format(LogicFunctionType)
                    ),
                    "template": LogicsTemplate,
                    "type": "code_editor",
                },
            )
            schema["properties"]["format"] = (
                {
                    "title": "format",
                    "description": (
                        "Formatting function. You should expose a function"
                        " named 'format' with following signature {}".format(
                            FormatFunctionType
                        )
                    ),
                    "template": FormatTemplate,
                    "type": "code_editor",
                },
            )

    @classmethod
    def with_default_format(
        cls, model_name: str, format: FormatFunctionType
    ) -> t.Type["BaseActionParameters"]:
        return create_model(
            model_name,
            format=(
                FormatFunctionType,
                Field(format, description=FormatDescription),
            ),
            __base__=cls,
        )


class WorkflowType(str, enum.Enum):
    catch = "catch"
    pull = "pull"


class ConnectorAction(BaseModel):
    WORKFLOW_FORMAT_PLACEHOLDER = "# << format_placeholder >>"
    WORKFLOW_LOGICS_PLACEHOLDER = "# << logics_placeholder >>"

    name: str
    type: WorkflowType
    description: str
    parameters: t.Type[BaseModel]
    source: Warehouse
    destination: Warehouse

    def workflow_code(self, connector_name: str) -> str:
        return WORKFLOW_TEMPLATE.render(
            format_placeholder=self.WORKFLOW_FORMAT_PLACEHOLDER,
            logics_placeholder=self.WORKFLOW_LOGICS_PLACEHOLDER,
            connector_name=connector_name,
            action_name=self.name,
            type=self.type.value,
            source_parameters=[
                parameter for parameter in self.source.pull.parameters.__fields__
            ],
            destination_parameters=[
                parameter for parameter in self.destination.push.parameters.__fields__
            ],
        )

    def run(
        self,
        connector_name: str,
        action_parameters: t.Dict,
        source_parameters: t.Dict,
        destination_parameters: t.Dict,
    ) -> ActionStatus:
        action_id = uuid.uuid4()
        adapter = ConnectorActionAdapter(
            logger,
            dict(
                log_tags=[
                    dict(name="connector", value=connector_name),
                    dict(name="action_name", value=self.name),
                    dict(name="action_id", value=action_id),
                ]
            ),
        )
        adapter.info("Starting Action")

        try:
            parameters = self.parameters(**action_parameters)
        except ValidationError as e:
            adapter.warning(
                "Failed to parse action_parameters with errors={}".format(e.errors())
            )
            return ActionStatus.failure

        try:
            source_parameters = self.source.pull.parameters(**source_parameters)
        except ValidationError as e:
            adapter.warning(
                "Failed to parse source_parameters with errors={}".format(e.errors())
            )
            return ActionStatus.failure

        try:
            destination_parameters = self.destination.push.parameters(
                **destination_parameters
            )
        except ValidationError as e:
            adapter.warning(
                "Failed to parse destination_parameters with errors={}".format(
                    e.errors()
                )
            )
            return ActionStatus.failure

        adapter.info(
            "Starting pulling from source={} with parameters={}".format(
                self.source.name, source_parameters
            )
        )
        source_adapter = ConnectorActionAdapter(
            logger,
            dict(
                log_tags=adapter.extra["log_tags"]
                + [
                    dict(name="warehouse", value=self.source.name),
                    dict(name="action", value="pull"),
                ]
            ),
        )
        try:
            source_items = list(self.source.pull(source_adapter, source_parameters))
        except Exception as e:
            adapter.error(
                "Failed pull from source={} with parameters={} error={}".format(
                    self.source.name, source_parameters, repr(e)
                )
            )
            return ActionStatus.failure
        adapter.info(
            "Finished pulling from source={} n_items={}".format(
                self.source.name, len(source_items)
            )
        )

        using_default_format = not bool(action_parameters.get("format"))
        adapter.info(
            "Starting formatting source items using {} function".format(
                "default" if using_default_format else "user defined"
            )
        )
        try:
            formatted_items = list(map(parameters.format, source_items))
        except Exception as e:
            adapter.error(
                "Failed to format source items using {} function error={}".format(
                    "default" if using_default_format else "user defined", repr(e)
                )
            )
            return ActionStatus.failure
        adapter.info("Finished formatting source items")

        if len(parameters.logics) > 0:
            adapter.info(
                "Applying logic functions: n_items={} before applying logics".format(
                    len(formatted_items)
                )
            )
            try:
                items_to_push = []
                for item in formatted_items:
                    for logic in parameters.logics:
                        item = logic(item)
                        if item is None:
                            break
                    else:
                        items_to_push.append(item)
            except Exception as e:
                adapter.error(
                    "Failed to apply logic functions error={}".format(repr(e))
                )
                return ActionStatus.failure
            adapter.info(
                "Logic functions applied: n_items={} after applying logics".format(
                    len(items_to_push)
                )
            )
        else:
            adapter.info("No logic functions supplied. Skipping")
            items_to_push = formatted_items

        adapter.info(
            "Starting pushing to destination={} with parameters={} n_items={}".format(
                self.destination.name, destination_parameters, len(items_to_push)
            )
        )
        destination_adapter = ConnectorActionAdapter(
            logger,
            dict(
                log_tags=adapter.extra["log_tags"]
                + [
                    dict(name="warehouse", value=self.destination.name),
                    dict(name="action", value="push"),
                ]
            ),
        )
        try:
            self.destination.push(
                destination_adapter, destination_parameters, items_to_push
            )
        except Exception as e:
            adapter.error(
                "Failed to push to destination={} with parameters={} error={}".format(
                    self.destination.name, destination_parameters, repr(e)
                )
            )
            return ActionStatus.failure
        adapter.info("Finished pushing to destination={}".format(self.destination.name))
        return ActionStatus.success


class ConnectorModel(BaseModel):
    name: str
    description: str
    url: str
    actions: t.List[ConnectorAction]


class Connector:
    def __init__(self, *args, **kwargs) -> None:
        self.model = ConnectorModel(*args, **kwargs)
        for action in self.model.actions:
            with_connector_name = partial(action.run, connector_name=self.model.name)
            setattr(self, action.name, with_connector_name)

    def manifest(self) -> t.Dict:
        model = self.model
        manifest = dict(name=model.name, actions=[])
        for action in model.actions:
            action_manifest = dict(
                name=action.name,
                action_parameters=action.parameters.schema(),
                source=action.source.name,
                source_parameters=action.source.pull.parameters.schema(),
                source_data_schema=action.source.data_schema.schema(),
                destination=action.destination.name,
                destination_parameters=action.destination.push.parameters.schema(),
                destination_data_schema=action.destination.data_schema.schema(),
                workflow_type=action.type,
                workflow_code=action.workflow_code(connector_name=model.name),
                workflow_code_format_placeholder=action.WORKFLOW_FORMAT_PLACEHOLDER,
                workflow_code_logics_placeholder=action.WORKFLOW_LOGICS_PLACEHOLDER,
            )
            manifest["actions"].append(action_manifest)
        return manifest


def hrflow_connectors_manifest(
    connectors: t.List[Connector], directory_path: str = "."
) -> None:
    manifest = dict(
        name="HrFlow.ai Connectors",
        connectors=[connector.manifest() for connector in connectors],
    )
    with open("{}/manifest.json".format(directory_path), "w") as f:
        json.dump(manifest, f, indent=2)
