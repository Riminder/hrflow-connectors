from math import inf


class WorkdayNumberOutOfBoundsError(Exception):
    def __init__(
        self, name: str, value: float, lb: float = -inf, ub: float = inf
    ) -> None:
        super().__init__(f"{name} must be between {lb} and {ub}, got {value}")
