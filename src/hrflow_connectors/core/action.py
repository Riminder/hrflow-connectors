from ..utils.clean_text import remove_html_tags

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
    hydrate_with_parsing: bool = False

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

    def hydrate_job_with_parsing(self, job: Dict[str, Any]) -> Dict[str, Any]:
        """
        Hydrate job with parsing

        Enrich the different fields of the job by applying parsing to large texts like `summary` or `sections`

        Args:
            data (Dict[str, Any]): job to hydrate

        Returns:
            Dict[str, Any]: hydrated job
        """
        # Concat `summary` text and each `section` together
        concatenated_str = job.get("summary")
        if concatenated_str is None:
            concatenated_str = ""

        section_list = job.get("sections")
        if section_list is not None:
            for section in section_list:
                section_description = section.get("description")
                if section_description is not None:
                    concatenated_str += "\n" + section_description

        # Clean the `concatenated_str` by removing htlm tags
        cleaned_str = remove_html_tags(concatenated_str)

        # If text is empty, the parsing can return an error
        if cleaned_str == "":
            return job

        # Parse the `cleaned`
        response = self.hrflow_client.document.parsing.post(text=cleaned_str)
        if response["code"] >= 300:
            raise RuntimeError("Parsing failed : `{}`".format(response["message"]))
        entity_list = response["data"]["ents"]
        parsed_text = response["data"]["text"]

        # Enrich job with parsing
        ## Initialize each field that can be enrich (case of None type)
        field_to_init = ["skills", "languages", "certifications", "courses", "tasks"]
        for field_name in field_to_init:
            field_value = job.get(field_name)
            if field_value is None:
                job[field_name] = []

        ## Map parsing labels with job fields
        label_to_job_field = dict(
            Course="courses",
            Task="tasks",
            Certification="certifications",
            Language="languages",
        )
        for entity in entity_list:
            start = entity["start"]
            end = entity["end"]
            label = entity["label"]
            selection = parsed_text[start:end]

            if label in ["Course", "Task", "Certification", "Language"]:
                field_to_add = dict(name=selection, value=None)
                job[label_to_job_field[label]].append(field_to_add)
            elif label in ["Skill", "HardSkill", "SoftSkill"]:
                label_to_skill_type = dict(
                    Skill=None, HardSkill="hard", SoftSkill="soft"
                )
                skill_type = label_to_skill_type[label]
                skill = dict(name=selection, type=skill_type, value=None)
                job["skills"].append(skill)

        return job

    def execute(self):
        """
        Execute action
        """
        input_data = self.pull()

        filtered_data = self.apply_logics(input_data)

        # connect each filtered_data to the format accepted by the pull function (destination, source, board)
        output_data = map(self.format, filtered_data)

        if self.hydrate_with_parsing:
            output_data = map(self.hydrate_job_with_parsing, output_data)

        self.push(output_data)