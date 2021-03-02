from hrflow import Hrflow
import requests


def workflow(body: dict, settings: dict)-> None:
    """
    CATCH WORKFLOW allows you to run a code function given an API POST request
    @param body: POST request Body
    @rtype: null
    @param settings: dictionary of settings params of the workflow
    """
