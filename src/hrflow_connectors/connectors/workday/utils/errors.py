from math import inf


class WorkdayNumberOutOfBoundsError(Exception):
    def __init__(
        self, name: str, value: float, lb: float = -inf, ub: float = inf
    ) -> None:
        super().__init__(f"{name} must be between {lb} and {ub}, got {value}")


class WorkdayFileNameTooLongError(Exception):
    def __init__(self, name: str, max: int, len: int) -> None:
        super().__init__(
            f"{name} expected to be at most {max} characters long, got {len}"
        )
