class Connector:
    """
    Abstract class `Connector`
    """

    def pull_jobs(self, *args, **kwargs):
        """
        Pull jobs
        """
        raise NotImplementedError("`pull_jobs` method is not implemented !")

    def pull_profiles(self, *args, **kwargs):
        """
        Pull profiles
        """
        raise NotImplementedError("`pull_profiles` method is not implemented !")

    def push_job(self, *args, **kwargs):
        """
        Push job
        """
        raise NotImplementedError("`push_job` method is not implemented !")

    def push_profile(self, *args, **kwargs):
        """
        Push profile
        """
        raise NotImplementedError("`push_profile` method is not implemented !")