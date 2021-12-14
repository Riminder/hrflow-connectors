from ..utils.clean_text import remove_html_tags
from ..utils.hrflow import find_element_in_list

from pydantic import BaseModel, Field
from typing import List, Dict, Any, Iterator, TypeVar
import itertools

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

    def get_all_job_pages_from_board(self) -> Iterator[Iterator[Dict[str, Any]]]:
        """
        Get all job pages from Board

        Yields:
            Iterator[Iterator[Dict[str, Any]]]: Iterator of job Iterator. For example: List of job list.
        """

        def get_jobs_page(page) -> Dict[str, Any]:
            response = self.hrflow_client.job.searching.list(
                board_keys=[self.board_key], limit=30, page=page
            )
            if response["code"] >= 300:
                raise RuntimeError(
                    "Hrflow searching failed : `{}`".format(response["message"])
                )
            return response

        job_page = get_jobs_page(page=1)
        max_page = job_page["meta"]["maxPage"]
        job_list = job_page["data"]["jobs"]
        yield job_list

        for page in range(2, max_page + 1):
            job_page = get_jobs_page(page=page)
            job_list = job_page["data"]["jobs"]
            if len(job_page["data"]["jobs"]) == 0:
                return
            yield job_list

    def get_all_references_from_board(self) -> Iterator[Dict[str, Any]]:
        """
        Get all job references from a Board

        Yields:
            Iterator[Dict[str, Any]]: Iterator with all job references
        """

        def get_reference_from_job_list(
            job_list: Iterator[Dict[str, Any]]
        ) -> Iterator[Dict[str, Any]]:
            """
            Get reference from job list

            Args:
                job_list (Iterator[Dict[str,Any]]): Job list

            Yields:
                Iterator[Dict[str, Any]]: Iteractor of references
            """
            get_reference_from_job = lambda job: job.get("reference")
            return map(get_reference_from_job, job_list)

        def none_filter(data: Dict[str, Any]) -> bool:
            """
            Filter None type

            Args:
                data (Dict[str, Any]): data to check

            Returns:
                bool: data is not None ?
            """
            return data is not None

        all_job_pages_iter = self.get_all_job_pages_from_board()
        all_reference_pages_iter = map(get_reference_from_job_list, all_job_pages_iter)
        chain_reference_iter = itertools.chain.from_iterable(all_reference_pages_iter)
        clean_iter = filter(none_filter, chain_reference_iter)
        return clean_iter

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
                element_to_add = dict(name=selection, value=None)
                # If element to add is unique. To avoid doublon.
                if (
                    find_element_in_list(
                        element_list=job[label_to_job_field[label]], name=selection
                    )
                    is None
                ):
                    job[label_to_job_field[label]].append(element_to_add)
            elif label in ["Skill", "HardSkill", "SoftSkill"]:
                label_to_skill_type = dict(
                    Skill=None, HardSkill="hard", SoftSkill="soft"
                )
                skill_type = label_to_skill_type[label]
                skill = dict(name=selection, type=skill_type, value=None)
                # If element to add is unique. To avoid doublon.
                if (
                    find_element_in_list(element_list=job["skills"], name=selection)
                    is None
                ):
                    job["skills"].append(skill)

        return job

    def check_reference_in_board(self, job: Dict[str, Any]) -> bool:
        """
        Check if a job reference is in the Board.
        If the job reference is not in the Board, return `True` to add the job.
        Otherwise return `False` and if job is archived, this function unarchives it.

        Args:
            job (Dict[str, Any]): job object

        Returns:
            bool: Job is in the Board
        """
        reference = job.get("reference")

        if reference is None:
            return True

        response = self.hrflow_client.job.indexing.get(
            board_key=self.board_key, reference=reference
        )
        response_code = response["code"]
        if response_code >= 400 and "Unable to find object: job" in response["message"]:
            # Job with this reference is not in the Board
            return True
        elif response_code >= 400:
            raise RuntimeError("Indexing get failed : {}".format(response["message"]))
        elif response_code == 200:
            job_in_board = response["data"]
            archived_at = job_in_board.get("archived_at")
            if archived_at is None:
                # Job is not archived
                return False
            else:
                # Job is archived
                self.hrflow_client.job.indexing.archive(
                    self.board_key, reference=reference, is_archive=0
                )
                return False
        return False

    def get_all_references_from_stream(self) -> Iterator[str]:
        """
        Get all job references from stream

        Yields:
            Iterator[str]: return all references
        """
        raise NotImplementedError("`get_all_references_from_stream` is not implemented")

    def check_deletion_references_from_stream(self):
        """
        Check the deletion of references from stream

        if reference in Board is missing in Stream
        Then archive the job with this reference
        """
        all_references_from_board_list = list(self.get_all_references_from_board())
        all_references_from_stream_list = list(self.get_all_references_from_stream())
        for reference in all_references_from_board_list:
            if reference not in all_references_from_stream_list:
                # Archive the job with this reference
                self.hrflow_client.job.indexing.archive(
                    self.board_key, reference=reference, is_archive=1
                )

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

        self.check_deletion_references_from_stream()

        unique_data_to_push = filter(self.check_reference_in_board, output_data)
        self.push(unique_data_to_push)