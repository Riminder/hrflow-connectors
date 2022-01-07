class Connector:
    """
    Abstract class `Connector`
    """
    @staticmethod
    def pull_jobs(*args, **kwargs):
        """
        Pull jobs
        """
        raise NotImplementedError("`pull_jobs` method is not implemented !")

    @staticmethod
    def pull_profiles(*args, **kwargs):
        """
        Pull profiles
        """
        raise NotImplementedError("`pull_profiles` method is not implemented !")

    @staticmethod
    def push_job(*args, **kwargs):
        """
        Push job
        """
        raise NotImplementedError("`push_job` method is not implemented !")

    @staticmethod
    def push_profile(*args, **kwargs):
        """
        Push profile
        """
        raise NotImplementedError("`push_profile` method is not implemented !")