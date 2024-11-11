import typing as t
from copy import deepcopy


class DB(list):
    def __init__(self, original: list[t.Any]):
        self.original = deepcopy(original)
        super().__init__(original)

    def reset(self):
        self.clear()
        self.extend(deepcopy(self.original))
