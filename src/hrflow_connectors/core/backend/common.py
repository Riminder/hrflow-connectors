import typing as t
from abc import ABC, abstractmethod

from pydantic import BaseModel


class BackendStore(ABC):
    @staticmethod
    @abstractmethod
    def NAME() -> str:
        raise NotImplementedError  # pragma: no cover

    @property
    def name(
        self,
    ) -> str:
        return self.NAME()

    @abstractmethod
    def save(self, key: str, data: BaseModel) -> None:
        raise NotImplementedError  # pragma: no cover

    @abstractmethod
    def load(self, key: str, parse_as: t.Type[BaseModel]) -> t.Optional[BaseModel]:
        raise NotImplementedError  # pragma: no cover
