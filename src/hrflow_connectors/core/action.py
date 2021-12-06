from pydantic import BaseModel, Field
from typing import List, Dict, Any, Iterator, TypeVar

Hrflow = TypeVar("Hrflow")


class Action(BaseModel):
    logics: List[str] = Field(
        [], description="Function names to apply as filter before pushing the data"
    )
    global_scope: Dict[str, Any] = None
    local_scope: Dict[str, Any] = None

    workflow_catch: bool = Field(
        True,
        const=True,
        description="Indicates if the action is executable in a workflow catch",
    )
    workflow_pull: bool = Field(
        True,
        const=True,
        description="Indicates if the action is executable in a workflow pull",
    )

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull data
        """
        raise NotImplementedError("`pull` is not implemented")

    def apply_logics(self, data: Iterator[Dict[str, Any]]) -> Iterator[Dict[str, Any]]:
        """
        Apply filters defined in `logics` on the `data` stream

        Args:
            data (List[Dict[str, Any]]): Data stream to filter

        Returns:
            List[Dict[str, Any]]: Filtered data stream
        """
        filtered_list = data
        for logic_function_name in self.logics:
            logic_function = eval(
                logic_function_name, self.global_scope, self.local_scope
            )
            filtered_list = filter(logic_function, filtered_list)
        return filtered_list

    def format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Format `data` fields to another field format.

        For example to select and transform only some fields from a database to Hrflow.
        This function must adapt the data schema passed in (from the `pull`) to the expected data schema in output (ready to be used in the `pull` function)

        Args:
            data (Dict[str, Any]): Data we want to adapt to the output format

        Returns:
            Dict[str, Any]: Data adapted to the input format of the pull function, ready to be sent
        """
        return data

    def push(self, data: Iterator[Dict[str, Any]]):
        """
        Push data

        Args:
            data (List[Dict[str, Any]]): Data to push
        """
        raise NotImplementedError("`push` is not implemented")

    def execute(self):
        """
        Execute action
        """
        input_data = self.pull()

        filtered_data = self.apply_logics(input_data)

        # connect each filtered_data to the format accepted by the pull function (destination, source, board)
        output_data = map(self.format, filtered_data)
        self.push(output_data)


class BoardAction(Action):
    hrflow_client: Hrflow
    board_key: str

    class Config:
        # `Hrflow` class is arbitrary type and can not be use without this option
        arbitrary_types_allowed = True

    def push(self, data: Iterator[Dict[str, Any]]):
        for job in data:
            response = self.hrflow_client.job.indexing.add_json(
                board_key=self.board_key, job_json=job
            )
            if response["code"] >= 300:
                message = response["message"]
                raise ConnectionError("Failed to push ! Reason : `{}`".format(message))
