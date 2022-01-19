from pydantic import BaseModel, Field
from typing import List, Dict, Any, Iterator, TypeVar, Optional, Union, Tuple
import itertools
import xml.etree.ElementTree
import html

from ..utils.clean_text import remove_html_tags
from ..utils.hrflow import find_element_in_list, Profile, Job
from ..utils.hrflow import generate_workflow_response
from ..utils.logger import get_logger
from ..core.auth import Auth, NoAuth

Hrflow = TypeVar("Hrflow")
TalentDataType = Union[str, xml.etree.ElementTree.Element, Dict[str, Any]]

logger = get_logger()


class BaseAction(BaseModel):
    """
    Abstract class `Action`
    """

    hrflow_client: Hrflow = Field(
        ...,
        description="Hrflow client instance used to communicate with the Hrflow.ai API",
    )

    auth: Auth = Field(
        NoAuth(),
        description="Auth instance to identify and communicate with the platform",
    )

    logics: List[str] = Field(
        [], description="Function names to apply as filter before pushing the data"
    )
    global_scope: Optional[Dict[str, Any]] = Field(
        None, description="A dictionary containing the current scope's global variables"
    )
    local_scope: Optional[Dict[str, Any]] = Field(
        None, description="A dictionary containing the current scope's local variables"
    )

    format_function_name: Optional[str] = Field(
        None, description="Function name to format job before pushing"
    )

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

    def pull(self) -> Iterator[TalentDataType]:
        """
        Pull data
        """
        raise NotImplementedError("`pull` is not implemented")

    def apply_logics(self, data: Iterator[TalentDataType]) -> Iterator[TalentDataType]:
        """
        Apply filters defined in `logics` on the `data` stream

        Args:
            data (List[TalentDataType]): Data stream to filter

        Returns:
            List[TalentDataType]: Filtered data stream
        """
        filtered_list = data

        logic_count = len(self.logics)
        logger.info(f"Number of logic functions to apply : {logic_count}")
        if logic_count > 0:
            logger.info(f"Logic functions to apply : {self.logics}")

        for logic_function_name in self.logics:
            logger.info(
                f"Evaluating the logic function named `{logic_function_name}` ..."
            )
            logic_function = eval(
                logic_function_name, self.global_scope, self.local_scope
            )
            logger.info(
                f"The logic function named `{logic_function_name}` has been evaluated"
            )
            filtered_list = filter(logic_function, filtered_list)
        return filtered_list

    def format_switcher(self, data: TalentDataType) -> Dict[str, Any]:
        """
        Choose the right function to format the data and format the input data into a push-ready data schema.

        This function will :
        * Try to retrieve the external format function given in `format_function_name` and format the data with it
        * In the case where no external format function is given `format_function_name=None`,
          then the function will execute the internal format function `format` of the class.
          The `format` function can be overloaded to give it a defined behaviour.
        * If the `format` function of the class is not overloaded,
          the default behaviour of this function is to return the input data.

        WARNING: If you want to map the format function to rewrite a pipeline of an `Action` from a connector,
        you should use this function, not the `format` of the parent class.
        If you take `format` from the parent class, then `format_function_name` will be ignored
        and only `format` will be used to format the data.

        Args:
            data (TalentDataType): Data we want to adapt to the output format

        Returns:
            Dict[str, Any]: Data adapted to the input format of the push function, ready to be sent
        """
        if self.format_function_name is None:
            logger.debug("External format function is not defined")
            logger.debug("Using internal `format` function")
            return self.format(data)
        else:
            logger.debug(
                f"Evaluating the external format function `{self.format_function_name}` ..."
            )
            format_function = eval(
                self.format_function_name, self.global_scope, self.local_scope
            )
            logger.debug(
                f"The external format function `{self.format_function_name}` has been evaluated"
            )
            return format_function(data)

    def format(self, data: TalentDataType) -> Dict[str, Any]:
        """
        Format the input data into a push-ready data schema

        WARNING: If you want to map the format function to rewrite a pipeline of an `Action` from a connector,
        you should use the function `format_switcher`, not the `format` of the parent class.
        If you take `format` from the parent class, then `format_function_name` will be ignored
        and only `format` will be used to format the data.

        Args:
            data (TalentDataType): Data we want to adapt to the output format

        Returns:
            Dict[str, Any]: Data adapted to the input format of the push function, ready to be sent
        """
        return data

    def push(self, data: Iterator[Union[str, Dict[str, Any]]]):
        """
        Push data

        Args:
            data (List[Union[str, Dict[str, Any]]]): Data to push
        """
        raise NotImplementedError("`push` is not implemented")

    def execute(self) -> Optional[Dict[str, Any]]:
        """
        Execute action
        """
        logger.info("Start execution")

        logger.info("Pulling data...")
        input_data = self.pull()
        logger.info("Data has been pulled")

        logger.info("Applying logics...")
        filtered_data = self.apply_logics(input_data)
        logger.info("Logics have been applied")

        logger.info("Mapping format function...")
        output_data = map(self.format_switcher, filtered_data)
        logger.info("Format function has been mapped")

        logger.info("Pushing data...")
        self.push(output_data)
        logger.info("Data has been pushed")

        logger.info("All has been done for this connector !")


class PullBaseAction(BaseAction):
    """
    Pull Action
    """

    def execute(self) -> Optional[Dict[str, Any]]:
        logger.info("Start execution")

        logger.info("Pulling data...")
        input_data = self.pull()
        logger.info("Data has been pulled")

        logger.info("Mapping format function...")
        formatted_data = map(self.format_switcher, input_data)
        logger.info("Format function has been mapped")

        logger.info("Applying logics...")
        filtered_data = self.apply_logics(formatted_data)
        logger.info("Logics have been applied")

        logger.info("Pushing data...")
        self.push(filtered_data)
        logger.info("Data has been pushed")

        logger.info("All has been done for this connector !")


class PushBaseAction(BaseAction):
    """
    Push Action
    """

    def execute(self) -> Optional[Dict[str, Any]]:
        logger.info("Start execution")

        logger.info("Pulling data...")
        input_data = self.pull()
        logger.info("Data has been pulled")

        logger.info("Applying logics...")
        filtered_data = self.apply_logics(input_data)
        logger.info("Logics have been applied")

        logger.info("Mapping format function...")
        formatted_data = map(self.format_switcher, filtered_data)
        logger.info("Format function has been mapped")

        logger.info("Pushing data...")
        self.push(formatted_data)
        logger.info("Data has been pushed")

        logger.info("All has been done for this connector !")


class PullJobsBaseAction(PullBaseAction):
    """
    Pull jobs from an external stream to Hrflow.ai
    """

    board_key: str = Field(
        ..., description="Board key where the jobs to be added will be stored"
    )
    hydrate_with_parsing: bool = Field(False, description="Enrich the job with parsing")
    archive_deleted_jobs_from_stream: bool = Field(
        True,
        description="Archive Board jobs when they are no longer in the incoming job stream",
    )

    def get_all_job_pages_from_board(self) -> Iterator[Iterator[Dict[str, Any]]]:
        """
        Get all job pages from Board

        Yields:
            Iterator[Iterator[Dict[str, Any]]]: Iterator of job Iterator. For example: List of job list.
        """
        logger.info(f"Getting all job from Hrflow Board `{self.board_key}`")

        def get_jobs_page(page: int) -> Dict[str, Any]:
            logger.info(f"Getting page `{page}` from `{self.board_key}` board")
            response = self.hrflow_client.job.searching.list(
                board_keys=[self.board_key], limit=30, page=page
            )
            if response["code"] >= 400:
                raise RuntimeError(
                    "Hrflow searching failed : `{}`".format(response["message"])
                )
            return response

        job_page = get_jobs_page(page=1)
        max_page = job_page["meta"]["maxPage"]
        logger.info(f"Max page in board `{self.board_key}` : {max_page}")
        job_list = job_page["data"]["jobs"]
        job_count = len(job_list)
        logger.info(f"Number of jobs got in page `1` : {job_count}")
        yield job_list

        for page in range(2, max_page + 1):
            job_page = get_jobs_page(page=page)
            job_list = job_page["data"]["jobs"]
            job_count = len(job_list)
            logger.info(f"Number of jobs got in page `{page}` : {job_count}")
            if len(job_page["data"]["jobs"]) == 0:
                return
            yield job_list

    def get_all_references_from_board(self) -> Iterator[str]:
        """
        Get all job references from a Board

        Yields:
            Iterator[str]: Iterator with all job references
        """
        logger.info(f"Getting all references from Hrflow Board `{self.board_key}` ...")

        def get_reference_from_job_list(
            job_list: Iterator[Dict[str, Any]]
        ) -> Iterator[str]:
            """
            Get reference from job list

            Args:
                job_list (Iterator[Dict[str,Any]]): Job list

            Yields:
                Iterator[Dict[str, Any]]: Iteractor of references
            """
            get_reference_from_job = lambda job: job.get("reference")
            return map(get_reference_from_job, job_list)

        def none_filter(data: Optional[str]) -> bool:
            """
            Filter None type

            Args:
                data (Optional[str]): data to check

            Returns:
                bool: data is not None ?
            """
            return data is not None

        all_job_pages_iter = self.get_all_job_pages_from_board()
        logger.info("Mapping a function to extract references from list of job")
        all_reference_pages_iter = map(get_reference_from_job_list, all_job_pages_iter)
        logger.info("Chaining all pages of jobs")
        chain_reference_iter = itertools.chain.from_iterable(all_reference_pages_iter)
        logger.info("Filtering all `None` value in the chain")
        clean_iter = filter(none_filter, chain_reference_iter)
        logger.info("Iterator containing all references is ready !")
        return clean_iter

    def push(self, data: Iterator[Union[str, Dict[str, Any]]]):
        for job in data:
            reference = job.get("reference")
            logger.debug(
                f"Pushing a job ref=`{reference}` to Hrflow Board `{self.board_key}`"
            )
            response = self.hrflow_client.job.indexing.add_json(
                board_key=self.board_key, job_json=job
            )
            if response["code"] >= 400:
                message = response["message"]
                logger.error("Failed to push a job !")
                raise RuntimeError("Failed to push ! Reason : `{}`".format(message))

    def hydrate_job_with_parsing(self, job: Dict[str, Any]) -> Dict[str, Any]:
        """
        Hydrate job with parsing

        Enrich the different fields of the job by applying parsing to large texts like `summary` or `sections`

        Args:
            data (Dict[str, Any]): job to hydrate

        Returns:
            Dict[str, Any]: hydrated job
        """
        reference = job.get("reference")
        logger.debug(f"Hydrating the current job with parsing... (ref=`{reference}`)")

        # Concat `summary` text and each `section` together
        logger.debug("Concatenating job summary with job sections...")
        concatenated_str = job.get("summary")
        if concatenated_str is None:
            concatenated_str = ""

        section_list = job.get("sections")
        if section_list is not None:
            for section in section_list:
                section_description = section.get("description")
                if section_description is not None:
                    concatenated_str += "\n" + section_description
        logger.debug("Job summary with job sections have been concatenated")

        # Clean the `concatenated_str` by removing htlm tags
        logger.debug("Removing HTML tags in concatenated texts...")
        cleaned_str = remove_html_tags(concatenated_str)
        cleaned_str = html.unescape(cleaned_str)
        cleaned_str = cleaned_str.strip()
        logger.debug("HTML tags in concatenated texts have been removed")

        # If text is empty, the parsing can return an error
        if cleaned_str == "":
            logger.debug("Cleaned text is empty")
            return job

        # Parse the `cleaned`
        logger.debug("Parsing the cleaned text...")
        response = self.hrflow_client.document.parsing.post(text=cleaned_str)
        if response["code"] >= 400:
            logger.error("Fail to parse the cleaned text !")
            raise RuntimeError("Parsing failed : `{}`".format(response["message"]))
        logger.debug("Text has been parsed")

        entity_list = response["data"]["ents"]
        parsed_text = response["data"]["text"]
        entity_count = len(entity_list)
        logger.debug(f"Number of entity found by parsing : {entity_count}")

        # Enrich job with parsing
        logger.debug("Enriching the current job with parsing")
        ## Initialize each field that can be enrich (case of None type)
        logger.debug("Initializing `None` fields to enrich")
        field_to_init = ["skills", "languages", "certifications", "courses", "tasks"]
        for field_name in field_to_init:
            field_value = job.get(field_name)
            if field_value is None:
                logger.debug(
                    f"Field `{field_name}` has been initialized with an empty list"
                )
                job[field_name] = []

        ## Map parsing labels with job fields
        logger.debug("Mapping parsing label with job fields")
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
                    logger.debug(
                        f"Add the field `{label_to_job_field[label]}` with the name `{selection}`"
                    )
                    job[label_to_job_field[label]].append(element_to_add)
                else:
                    logger.debug(
                        f"The field `{label_to_job_field[label]}` with the name `{selection}` is already added"
                    )
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
                    logger.debug(
                        f"Add the skill `{skill_type}` with the name `{selection}`"
                    )
                    job["skills"].append(skill)
                else:
                    logger.debug(
                        f"The skill `{skill_type}` with the name `{selection}` is already added"
                    )

        logger.debug("The job has been enriched !")
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
        logger.debug(
            f"Checking if the reference `{reference}` is already in Hrflow Board `{self.board_key}`"
        )

        if reference is None:
            logger.debug("This job reference is `None`. Thus, it is not in the Board")
            return True

        logger.debug("Sending a GET Indexing request to Hrflow...")
        response = self.hrflow_client.job.indexing.get(
            board_key=self.board_key, reference=reference
        )
        response_code = response["code"]
        if response_code >= 400 and "Unable to find object: job" in response["message"]:
            # Job with this reference is not in the Board
            logger.debug("Hrflow does not find this job reference")
            return True
        elif response_code >= 400:
            logger.error("GET Indexing failed !")
            raise RuntimeError("GET Indexing failed : {}".format(response["message"]))
        elif response_code == 200:
            logger.debug("Hrflow got this job in the Board")
            job_in_board = response["data"]
            archived_at = job_in_board.get("archived_at")
            if archived_at is None:
                # Job is not archived
                logger.debug("Job is not archived")
                return False
            else:
                # Job is archived
                logger.debug(f"Job is archived since `{archived_at}`")
                logger.debug("Un-archiving the job in Hrflow")
                archive_response = self.hrflow_client.job.indexing.archive(
                    self.board_key, reference=reference, is_archive=0
                )
                if archive_response["code"] >= 400:
                    error_message = archive_response["message"]
                    logger.warning(
                        f"Fail to un-archive the job `{reference}` : {error_message}"
                    )
                else:
                    # Get job key
                    job_response = response["data"]
                    job_key = job_response["key"]

                    # Hydrate job with parsing if it is necessary
                    if self.hydrate_with_parsing:
                        job = self.hydrate_job_with_parsing(job)

                    # Edit job
                    logger.debug(f"Editing the job key=`{job_key}`")
                    edit_response = self.hrflow_client.job.indexing.edit(
                        board_key=self.board_key, key=job_key, job_json=job
                    )
                    if edit_response["code"] >= 400:
                        error_message = edit_response["message"]
                        logger.warning(
                            f"Fail to edit the job `{reference}` : {error_message}"
                        )
                return False
        return False

    def get_all_references_from_stream(self) -> Iterator[str]:
        """
        Get all job references from stream

        Yields:
            Iterator[str]: return all references
        """
        logger.info(f"Getting all references from the stream...")

        logger.info(f"Pulling all references from the stream")
        input_data = self.pull()

        logger.info(f"Formating all references from the stream")
        formatted_data = map(self.format_switcher, input_data)

        logger.info(f"Applying logics to all references from the stream")
        filtered_data = self.apply_logics(formatted_data)

        logger.info(f"Keeping only reference from the stream")
        references_iter = map(lambda job: job.get("reference"), filtered_data)

        references_without_none_iter = filter(
            lambda ref: ref is not None, references_iter
        )

        logger.info(f"All references from the stream have been got")
        return references_without_none_iter

    def check_deletion_references_from_stream(self):
        """
        Check the deletion of references from stream

        if reference in Board is missing in Stream
        Then archive the job with this reference
        """
        logger.info("Checking the deletion of references from stream...")
        all_references_from_board_list = list(self.get_all_references_from_board())
        all_references_from_stream_list = list(self.get_all_references_from_stream())
        for reference in all_references_from_board_list:
            if reference not in all_references_from_stream_list:
                # Archive the job with this reference
                logger.debug(f"Archiving the job `{reference}`")
                archive_response = self.hrflow_client.job.indexing.archive(
                    self.board_key, reference=reference, is_archive=1
                )
                if archive_response["code"] >= 400:
                    error_message = archive_response["message"]
                    logger.warning(
                        f"Fail to archive the job `{reference}` : {error_message}"
                    )

        logger.info("The deletion of references from stream has been checked")

    def execute(self) -> Optional[Dict[str, Any]]:
        """
        Execute action
        """
        logger.info("Start execution")

        logger.info("Pulling data...")
        input_data = self.pull()
        logger.info("Data has been pulled")

        logger.info("Mapping format function...")
        formatted_data = map(self.format_switcher, input_data)
        logger.info("Format function has been mapped")

        logger.info("Applying logics...")
        filtered_data = self.apply_logics(formatted_data)
        logger.info("Logics have been applied")

        logger.info(
            f"Archive the deleted jobs from stream : {self.archive_deleted_jobs_from_stream}"
        )
        if self.archive_deleted_jobs_from_stream:
            self.check_deletion_references_from_stream()

        logger.info("Filtering the job to push")
        unique_data_to_push = filter(self.check_reference_in_board, filtered_data)

        logger.info(f"Hydrate job with parsing : {self.hydrate_with_parsing}")
        if self.hydrate_with_parsing:
            logger.info("Mapping hydrate_job_with_parsing function...")
            unique_data_to_push = map(
                self.hydrate_job_with_parsing, unique_data_to_push
            )
            logger.info("hydrate_job_with_parsing function has been mapped")

        logger.info("Pushing data...")
        self.push(unique_data_to_push)
        logger.info("Data has been pushed")

        logger.info("All has been done for this connector !")


class PushJobBaseAction(PushBaseAction):

    job: Job = Field(..., description="Job to push")

    def pull(self) -> Iterator[Dict[str, Any]]:
        """
        Pull data
        """
        response = self.hrflow_client.job.indexing.get(
            board_key=self.job.board.key, key=self.job.key
        )
        if response["code"] >= 400:
            raise RuntimeError(
                "Indexing job get failed : `{}`".format(response["message"])
            )

        job = response["data"]
        return [job]

    def execute(self):
        super().execute()
        return generate_workflow_response(
            status_code=201, message="Profile successfully pushed"
        )


class PushProfileBaseAction(PushBaseAction):
    profile: Profile = Field(..., description="Profile to push")

    def pull(self) -> Iterator[TalentDataType]:
        """
        Pull data
        """
        logger.info(
            f"Pulling a profile key=`{self.profile.key}` from a Source `{self.profile.source.key}`"
        )
        response = self.hrflow_client.profile.indexing.get(
            source_key=self.profile.source.key, key=self.profile.key
        )
        if response["code"] >= 400:
            logger.error("Fail to pull a profile")
            raise RuntimeError(
                "Indexing profile get failed : `{}`".format(response["message"])
            )

        profile = response["data"]
        return [profile]

    def execute(self):
        super().execute()
        return generate_workflow_response(
            status_code=201, message="Profile successfully pushed"
        )


class CatchProfileBaseAction(BaseAction):

    source_key: str = Field(
        ..., description="Source key where the profiles to be added will be stored"
    )

    request: Dict[str, Any] = Field(..., description="Body to format in HrFlow Profile")

    def execute(self):
        """
        Execute action
        """
        logger.info("Start execution")

        logger.info("Format request...")
        formatted_data = self.format_switcher(self.request)
        logger.info("Request has been formatted")

        logger.info("Pushing data...")
        self.push(formatted_data)
        logger.info("Data has been pushed")

        logger.info("All has been done for this connector !")
        return generate_workflow_response(
            status_code=201, message="Profile successfully pushed"
        )

    def push(self, data: Dict[str, Any]):
        logger.debug(f"Parsing a profile to Hrflow Source `{self.source_key}`")
        response = self.hrflow_client.profile.parsing.add_file(**data)
        if response["code"] >= 400:
            message = response["message"]
            logger.error("Failed to push a profile !")
            raise RuntimeError("Failed to push ! Reason : `{}`".format(message))
