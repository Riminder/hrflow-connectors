from pydantic import BaseModel, Field
from typing import List, Dict, Any

class Action(BaseModel):
    logics: List[str] = Field([], description="Function names to apply as filter before pushing the data")
    global_scope: Dict[str, Any] = None
    local_scope: Dict[str, Any] = None

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
        filtered_list = data
        for logic_function_name in self.logics:
            logic_function = eval(logic_function_name, self.global_scope, self.local_scope)
            filtered_list = list(filter(logic_function, filtered_list))
        return filtered_list
    
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