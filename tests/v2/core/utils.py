import random
import string


def random_workflow_id() -> str:
    return "".join([random.choice(string.ascii_letters) for _ in range(10)])
