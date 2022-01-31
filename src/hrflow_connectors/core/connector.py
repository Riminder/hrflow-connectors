from typing import Optional, Dict, Any
from hrflow import Hrflow

class Connector:
    """
    Abstract class `Connector`
    """

    @staticmethod
    def pull_jobs(hrflow_client: Hrflow, *args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Pull jobs
        """
        raise NotImplementedError("`pull_jobs` method is not implemented !")

    @staticmethod
    def pull_profiles(hrflow_client: Hrflow, *args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Pull profiles
        """
        raise NotImplementedError("`pull_profiles` method is not implemented !")

    @staticmethod
    def push_job(hrflow_client: Hrflow, *args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Push job
        """
        raise NotImplementedError("`push_job` method is not implemented !")

    @staticmethod
    def push_profile(hrflow_client: Hrflow, *args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Push profile
        """
        raise NotImplementedError("`push_profile` method is not implemented !")

    @staticmethod
    def catch_profile(hrflow_client: Hrflow, *args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Catch profile
        """
        raise NotImplementedError("`catch_profile` method is not implemented !")
