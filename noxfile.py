import tempfile
from contextlib import contextmanager

import nox

nox.options.reuse_existing_virtualenvs = True


@contextmanager
def requirements_file(session) -> None:
    with tempfile.NamedTemporaryFile() as requirements:
        session.run(
            "poetry",
            "export",
            "--dev",
            "--without-hashes",
            "--format=requirements.txt",
            f"--output={requirements.name}",
            external=True,
        )
        yield requirements


@nox.session(python=["3.7", "3.8", "3.9", "3.10", "3.11"])
def core(session):
    with requirements_file(session) as requirements:
        session.install("-r", requirements.name)
        session.run(
            "pytest",
            "-x",
            env={
                "HRFLOW_CONNECTORS_STORE_ENABLED": "1",
                "HRFLOW_CONNECTORS_LOCALJSON_DIR": "/tmp/",
                "PYTHONPATH": "./src/",
            },
        )
