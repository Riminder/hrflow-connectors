from typing import Optional, Dict, Any


class Connector:
    """
    Abstract class `Connector`
    """

    @staticmethod
    def pull_jobs(*args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Pull jobs
        """
        raise NotImplementedError("`pull_jobs` method is not implemented !")

    @staticmethod
    def pull_profiles(*args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Pull profiles
        """
        raise NotImplementedError("`pull_profiles` method is not implemented !")

    @staticmethod
    def push_job(*args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Push job
        """
        raise NotImplementedError("`push_job` method is not implemented !")

    @staticmethod
    def push_profile(*args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Push profile
        """
        raise NotImplementedError("`push_profile` method is not implemented !")

    @staticmethod
    def catch_profile(*args, **kwargs) -> Optional[Dict[str, Any]]:
        """
        Catch profile
        """
        raise NotImplementedError("`catch_profile` method is not implemented !")
