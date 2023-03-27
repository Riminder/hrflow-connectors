import json
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
            env={
                "HRFLOW_CONNECTORS_STORE_ENABLED": "1",
                "HRFLOW_CONNECTORS_LOCALJSON_DIR": "/tmp/",
                "PYTHONPATH": "./src/",
            },
        )


PRODUCE_MANIFEST_IN_DIRECTORY = (
    "from hrflow_connectors import __CONNECTORS__, hrflow_connectors_manifest as m;"
    " m(connectors=__CONNECTORS__, directory_path='{directory}')"
)


@nox.session(python=["3.7", "3.8", "3.9", "3.10", "3.11"])
def manifest(session):
    with open("./manifest.json", "r") as f:
        current_manifest = json.load(f)

    with requirements_file(session) as requirements:
        session.install("-r", requirements.name)

    with tempfile.TemporaryDirectory() as directory:
        session.run(
            "python",
            "-c",
            PRODUCE_MANIFEST_IN_DIRECTORY.format(directory=directory),
            env={
                "PYTHONPATH": "./src/",
            },
        )
        with open(f"{directory}/manifest.json", "rb") as f:
            produced_manifest_binary = f.read()
        produced_manifest = json.loads(produced_manifest_binary.decode())
        if current_manifest != produced_manifest:
            with tempfile.NamedTemporaryFile("wb", delete=False) as produced_manifest:
                produced_manifest.write(produced_manifest_binary)
            session.error(
                f"Produced manifest for python{session.python} is "
                "different than reference\n"
                f"File can be inspected at '{produced_manifest.name}'"
            )
