import enum
import typing as t
from logging import LoggerAdapter

from pydantic import BaseModel, Field, ValidationError, create_model, root_validator


class FieldNotFoundError(RuntimeError):
    pass


class FixedValueValidationError(RuntimeError):
    pass


class DataType(enum.Enum):
    profile = enum.auto()
    job = enum.auto()
    other = enum.auto()


class ActionType(enum.Enum):
    read = enum.auto()
    write = enum.auto()


class ReadMode(enum.Enum):
    sync = "sync"
    incremental = "incremental"


class ActionEndpoints(BaseModel):
    name: str
    description: str
    url: str


class WarehouseReadAction(BaseModel):
    endpoints: t.List[ActionEndpoints] = Field(default_factory=list)
    parameters: t.Type[BaseModel]
    function: t.Callable[
        [LoggerAdapter, BaseModel, t.Optional[ReadMode], t.Optional[str]],
        t.Iterable[t.Dict],
    ]
    item_to_read_from: t.Optional[t.Callable[[t.Dict], str]] = None
    supports_incremental: bool = False

    def __call__(self, *args, **kwargs) -> t.Iterable[t.Dict]:
        return self.function(*args, **kwargs)

    @root_validator
    def validate_incremental(cls, values):
        supports_incremental = values.get("supports_incremental")
        item_to_read_from = values.get("item_to_read_from")
        if supports_incremental is True and item_to_read_from is None:
            raise ValueError(
                "Function item_to_read_from must be provided when"
                " supports_incremental is True"
            )
        return values


class WarehouseWriteAction(BaseModel):
    endpoints: t.List[ActionEndpoints] = Field(default_factory=list)
    parameters: t.Type[BaseModel]
    function: t.Callable[[LoggerAdapter, BaseModel, t.Iterable[t.Dict]], t.List[t.Dict]]

    def __call__(self, *args, **kwargs) -> t.List[t.Dict]:
        return self.function(*args, **kwargs)


class Warehouse(BaseModel):
    name: str
    data_type: DataType
    data_schema: t.Type[BaseModel] = Field(default_factory=lambda: BaseModel)
    read: t.Optional[WarehouseReadAction]
    write: t.Optional[WarehouseWriteAction]

    @property
    def supports_incremental(self):
        return self.read.supports_incremental

    def item_to_read_from(self, *args, **kwargs):
        return self.read.item_to_read_from(*args, **kwargs)

    @property
    def is_readable(self):
        return self.read is not None

    @property
    def is_writable(self):
        return self.write is not None

    def with_fixed_read_parameters(self, **tofix) -> "Warehouse":
        return self.__with_fixed_parameters(action_type=ActionType.read, **tofix)

    def with_fixed_write_parameters(self, **tofix) -> "Warehouse":
        return self.__with_fixed_parameters(action_type=ActionType.write, **tofix)

    def __with_fixed_parameters(self, action_type: ActionType, **tofix) -> "Warehouse":
        action_to_fix = getattr(self, action_type.name)
        fixed = dict()
        original_fields = action_to_fix.parameters.__fields__
        for field, value in tofix.items():
            if field not in original_fields:
                raise FieldNotFoundError(
                    "The field you are trying to fix '{}' is not part of the available"
                    " parameters {}".format(field, list(original_fields.keys()))
                )
            try:
                action_to_fix.parameters(**{field: value})
            except ValidationError as e:
                errors = e.errors()
                field_error = next(
                    (error for error in errors if error["loc"] == (field,)), None
                )
                if field_error is not None:
                    raise FixedValueValidationError(
                        "The value='{}' you are trying to use for field='{}' does not"
                        " pass the original validation with error={}".format(
                            value, field, field_error
                        )
                    )
            original = action_to_fix.parameters.__fields__[field]
            fixed[field] = (
                original.type_,
                Field(value, const=True, description=original.field_info.description),
            )
        with_fixed_parameters = create_model(
            "Fixed{}Parameters".format(action_type.name.capitalize()),
            __base__=action_to_fix.parameters,
            **fixed
        )
        if action_type is ActionType.read:
            return Warehouse(
                name=self.name,
                data_schema=self.data_schema,
                data_type=self.data_type,
                read=WarehouseReadAction(
                    endpoints=self.read.endpoints,
                    parameters=with_fixed_parameters,
                    function=self.read.function,
                ),
                write=self.write,
            )

        return Warehouse(
            name=self.name,
            data_schema=self.data_schema,
            data_type=self.data_type,
            read=self.read,
            write=WarehouseWriteAction(
                endpoints=self.write.endpoints,
                parameters=with_fixed_parameters,
                function=self.write.function,
            ),
        )
