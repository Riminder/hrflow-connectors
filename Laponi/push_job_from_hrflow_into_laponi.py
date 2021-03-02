from hrflow import Hrflow
import requests


def workflow(body: dict, settings: dict) -> None:
    """
    PULL WORKFLOW allows you to run the following code instructions on a regular basis

    @rtype: null
    @param body: POST request Body
    @param settings: dictionary of settings params of the workflow
    """