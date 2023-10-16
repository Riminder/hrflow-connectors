class UKGProEmptyFileName(Exception):
    pass


class UKGProEmptyString(Exception):
    pass


class UKGProAuthenticationError(Exception):
    def __init__(self, reason: str) -> None:
        super().__init__(reason)
