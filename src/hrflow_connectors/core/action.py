from pydantic import BaseModel, Field
from typing import List, Dict, Any

class Action(BaseModel):
    logics: List[str] = Field([], description="Function names to apply as filter before pushing the data")
    workflow_catch: bool = Field(True, const=True, description="Indicates if the action is executable in a workflow catch")
    workflow_pull: bool = Field(True, const=True, description="Indicates if the action is executable in a workflow pull")

    def pull(self) ->  List[Dict[str, Any]]:
        """
        Pull data
        """
        raise NotImplementedError("`pull` is not implemented")
    
    def apply_logics(self, data : List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Apply filters defined in `logics` on the `data` stream

        Args:
            data (List[Dict[str, Any]]): Data stream to filter

        Returns:
            List[Dict[str, Any]]: Filtered data stream
        """
        raise NotImplementedError("`apply_logics` is not implemented")
    
    def push(self, data : List[Dict[str, Any]]):
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
        raise NotImplementedError("`execute` is not implemented")