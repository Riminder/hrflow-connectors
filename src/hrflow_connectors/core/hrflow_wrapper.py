from pydantic import BaseModel
from typing import List, Optional, Dict, Any


class JobWrapper(BaseModel):
    key: Optional[str] = None
    reference: Optional[str] = None
    name: str
    location: Dict[str, Any]
    sections: Optional[List[Dict[str, Any]]] = None
    url: Optional[str] = None
    summary: Optional[str] = None
    board: Optional[Dict[str, Any]] = None
    archived_at: Optional[str] = None
    updated_at: Optional[str] = None
    created_at: Optional[str] = None
    skills: Optional[List[Dict[str, Any]]] = None
    languages: Optional[List[Dict[str, Any]]] = None
    certifications: Optional[List[Dict[str, Any]]] = None
    courses: Optional[List[Dict[str, Any]]] = None
    tasks: Optional[List[Dict[str, Any]]] = None
    tags: Optional[List[Dict[str, Any]]] = None
    metadatas: Optional[List[Dict[str, Any]]] = None
    ranges_float: Optional[List[Dict[str, Any]]] = None
    ranges_date: Optional[List[Dict[str, Any]]] = None

    ######################
    ### GENERIC FIELDS ###
    ######################

    def is_in_fields(
        self, field_list: Optional[List[Dict[str, Any]]], name: str
    ) -> bool:
        """
        Check if the field with the field `name` is present in job fields

        Args:
            field_list (Optional[List[Dict[str, Any]]]) : list of fields with `name` & `value`
            name (str): field name

        Returns:
            bool: field is present ?
        """
        if field_list is None:
            return False
        for field in field_list:
            if field.get("name") == name:
                return True
        return False

    def get_field_value(
        self,
        field_list: Optional[List[Dict[str, Any]]],
        name: str,
        default_value: Any = None,
    ) -> Any:
        """
        get field value with the field `name`.
        If the field does not exist, return the `default_value`.

        Args:
            field_list (Optional[List[Dict[str, Any]]]) : list of fields with `name` & `value`
            name (str): field name
            default_value (Any, optional): default value. Defaults to None.

        Returns:
            Any: return field value
        """
        if field_list is None:
            return default_value
        for field in field_list:
            if field.get("name") == name:
                return field.get("value")
        return default_value

    def add_field(
        self, field_list: Optional[List[Dict[str, Any]]], name: str, value: Any
    ) -> Optional[List[Dict[str, Any]]]:
        """
        Append a field to the list of fields even if the field name already exists

        Args:
            field_list (Optional[List[Dict[str, Any]]]) : list of fields with `name` & `value`
            name (str): field name
            value (Any): field value
        Returns:
            Optional[List[Dict[str, Any]]]: return new fields state
        """
        field = dict(name=name, value=value)
        if field_list is None:
            field_list = []
        field_list.append(field)
        return field_list

    def set_field(
        self, field_list: Optional[List[Dict[str, Any]]], name: str, value: Any
    ) -> None:
        """
        Set the field with the field `name`.
        If field does not exist, create the field.

        Args:
            field_list (Optional[List[Dict[str, Any]]]) : list of fields with `name` & `value`
            name (str): field name
            value (Any): field value
        """
        if field_list is not None:
            for field in field_list:
                if field.get("name") == name:
                    field["value"] = value
                    return field_list
        # Créé le tag si pas existant
        return self.add_field(field_list, name, value)

    ############
    ### TAGS ###
    ############

    def is_in_tags(self, name: str) -> bool:
        """
        Check if the tag with the `name` is present in job tags

        Args:
            name (str): tag name

        Returns:
            bool: tag is present ?
        """
        return self.is_in_fields(self.tags, name)

    def add_tag(self, name: str, value: Any) -> None:
        """
        Append a tag to the list of tag even if the tag name already exists

        Args:
            name (str): tag name
            value (Any): tag value
        """
        self.tags = self.add_field(self.tags, name, value)

    def set_tag(self, name: str, value: Any) -> None:
        """
        Set the tag with the tag `name`.
        If tag does not exist, create the tag.

        Args:
            name (str): tag name
            value (Any): tag value
        """
        self.tags = self.set_field(self.tags, name, value)

    def get_tag(self, name: str, default_value: Any = None) -> Any:
        """
        get tag value with the tag `name`.
        If the tag does not exist, return the `default_value`.

        Args:
            name (str): tag name
            default_value (Any, optional): default value. Defaults to None.

        Returns:
            Any: return tag value
        """
        return self.get_field(self.tags, name, default_value)

    ################
    ### METADATA ###
    ################

    def is_in_metadatas(self, name: str) -> bool:
        """
        Check if the metadata with the metadata `name` is present in job metadata

        Args:
            name (str): metadata name

        Returns:
            bool: metadata is present ?
        """
        return self.is_in_fields(self.metadatas, name)

    def get_meta(self, name: str, default_value: Any = None) -> Any:
        """
        get metadata value with the metadata `name`.
        If the metadata does not exist, return the `default_value`.

        Args:
            name (str): metadata name
            default_value (Any, optional): default value. Defaults to None.

        Returns:
            Any: return metadata value
        """
        return self.get_field(self.metadatas, name, default_value)

    def add_meta(self, name: str, value: Any) -> None:
        """
        Append a metadata to the list of metadata even if the metadata name already exists

        Args:
            name (str): metadata name
            value (Any): metadata value
        """
        self.metadatas = self.add_meta(self.metadatas, name, value)

    def set_meta(self, name: str, value: Any) -> None:
        """
        Set the metadata with the metadata `name`.
        If metadata does not exist, create the metadata.

        Args:
            name (str): metadata name
            value (Any): metadata value
        """
        self.metadatas = self.set_field(self.metadatas, name, value)

    ###############
    ### SECTION ###
    ###############

    def get_section(self, name: str, default_description: Optional[str] = None) -> Any:
        """
        Get the section object with the section `name`.
        If the section does not exist, return the `default_value`.

        Args:
            name (str): section name
            default_value (Any): default section object. Defaults to None.

        Returns:
            Any: the section object
        """
        if self.sections is None:
            return default_description
        for section in self.sections:
            if section["name"] == name:
                return section.get("description")
        return default_description

    def add_section(self, name: str, title: Optional[str], description: Optional[str]):
        """
        Append a section to the list of section even if the metadata name already exists

        Args:
            name (str): section name
            title (Optional[str]): section title
            description (Optional[str]): section description
        """
        section = dict(name=name, title=title, description=description)
        if self.sections is None:
            self.sections = []
        self.sections.append(section)

    def set_section(
        self, name: str, description: Optional[str], title: Optional[str] = None
    ):
        """
        Set the section with the section `name`.
        If section does not exist, create the section.

        Args:
            name (str): section name
            title (Optional[str]): section title
            description (Optional[str]): section description
        """
        if self.sections is not None:
            for section in self.sections:
                if section["name"] == name:
                    if title is not None:
                        section["title"] = title
                    section["description"] = description
                    return
        self.add_section(name=name, description=description, title=title)

    ###################
    ### RANGES DATE ###
    ###################

    def get_range_date(
        self, name: str, default_value: Optional[Dict[str, Any]] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Get the range date object with the range date `name`.
        If the range date  does not exist, return the `default_value`.

        Args:
            name (str): range date name
            default_value (Optional[Dict[str, Any]]): default range date object. Defaults to None.

        Returns:
            Optional[Dict[str, Any]]: the range date object
        """
        if self.ranges_date is None:
            return default_value
        for range_date in self.ranges_date:
            if range_date["name"] == name:
                return range_date
        return default_value

    ####################
    ### RANGES FLOAT ###
    ####################

    def get_range_float(
        self, name: str, default_value: Optional[Dict[str, Any]] = None
    ) -> Optional[Dict[str, Any]]:
        """
        Get the range float object with the range float `name`.
        If the range float  does not exist, return the `default_value`.

        Args:
            name (str): range float name
            default_value (Optional[Dict[str, Any]]): default range float object. Defaults to None.

        Returns:
            Optional[Dict[str, Any]]: the range float object
        """
        if self.ranges_float is None:
            return default_value
        for range_float in self.ranges_float:
            if range_float["name"] == name:
                return range_float
        return default_value
