import difflib
import inspect
import json
import os
import tempfile
import typing as t
from contextlib import contextmanager
from pathlib import Path

import nox

nox.options.reuse_existing_virtualenvs = True

PYTHON_VERSIONS = ["3.7", "3.8", "3.9", "3.10", "3.11"]
REQUIREMENTS_CONTENT = {}


@contextmanager
def requirements_file(session, s3_extra: bool = False) -> None:
    global REQUIREMENTS_CONTENT
    extra_args = tuple()
    if s3_extra is True:
        extra_args = ("-E", "s3")

    if REQUIREMENTS_CONTENT.get(s3_extra) is None:
        with tempfile.NamedTemporaryFile("rb") as requirements:
            session.run(
                "poetry",
                "export",
                "--dev",
                *extra_args,
                "--without-hashes",
                "--format=requirements.txt",
                f"--output={requirements.name}",
                external=True,
            )
            REQUIREMENTS_CONTENT[s3_extra] = requirements.read()
            yield requirements
    else:
        with tempfile.NamedTemporaryFile("wb") as requirements:
            requirements.write(REQUIREMENTS_CONTENT[s3_extra])
            yield requirements


@nox.session(python=PYTHON_VERSIONS)
def tests(session):
    with requirements_file(session) as requirements:
        session.install("-r", requirements.name)
        session.run(
            "pytest",
            *session.posargs,
            env={
                "HRFLOW_CONNECTORS_STORE_ENABLED": "1",
                "HRFLOW_CONNECTORS_LOCALJSON_DIR": "/tmp/",
                "PYTHONPATH": "./src/",
            },
        )


@nox.session(python=PYTHON_VERSIONS)
def tests_s3(session):
    with requirements_file(session, s3_extra=True) as requirements:
        session.install("-r", requirements.name)
        session.run(
            "pytest",
            *session.posargs,
            env={
                "HRFLOW_CONNECTORS_STORE_ENABLED": "1",
                "HRFLOW_CONNECTORS_LOCALJSON_DIR": "/tmp/",
                "S3_STORE_TEST_BUCKET": os.getenv("S3_STORE_TEST_BUCKET"),
                "S3_STORE_TEST_AWS_REGION": os.getenv("S3_STORE_TEST_AWS_REGION"),
                "S3_STORE_TEST_AWS_ACCESS_KEY_ID": os.getenv(
                    "S3_STORE_TEST_AWS_ACCESS_KEY_ID"
                ),
                "S3_STORE_TEST_AWS_SECRET_ACCESS_KEY": os.getenv(
                    "S3_STORE_TEST_AWS_SECRET_ACCESS_KEY"
                ),
                "S3_STORE_TEST_READ_ONLY_AWS_ACCESS_KEY_ID": os.getenv(
                    "S3_STORE_TEST_READ_ONLY_AWS_ACCESS_KEY_ID"
                ),
                "S3_STORE_TEST_READ_ONLY_AWS_SECRET_ACCESS_KEY": os.getenv(
                    "S3_STORE_TEST_READ_ONLY_AWS_SECRET_ACCESS_KEY"
                ),
                "S3_STORE_TEST_WRITE_ONLY_AWS_ACCESS_KEY_ID": os.getenv(
                    "S3_STORE_TEST_WRITE_ONLY_AWS_ACCESS_KEY_ID"
                ),
                "S3_STORE_TEST_WRITE_ONLY_AWS_SECRET_ACCESS_KEY": os.getenv(
                    "S3_STORE_TEST_WRITE_ONLY_AWS_SECRET_ACCESS_KEY"
                ),
                "PYTHONPATH": "./src/",
            },
        )


PRODUCE_MANIFEST_IN_DIRECTORY = (
    "from hrflow_connectors import __CONNECTORS__, hrflow_connectors_manifest as m;"
    " m(connectors=__CONNECTORS__, directory_path='{directory}')"
)


@nox.session(python=PYTHON_VERSIONS)
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


GENERATE_DOCUMENTATION = """
import json
import typing as t

{GENERATE_DIGEST_SOURCE}

digest, content = {GENARATE_DIGEST_FUNC_NAME}()
with open("{save_digest_to}", "wt") as f:
    json.dump(digest, f)
with open("{save_content_to}", "wt") as f:
    json.dump(content, f)
"""


def generate_doc_digest() -> t.Tuple[dict, dict]:
    import hashlib
    from collections import defaultdict
    from unittest import mock

    from hrflow_connectors import __CONNECTORS__, generate_docs
    from hrflow_connectors.core import documentation

    doc_digest = defaultdict(lambda: defaultdict(dict))
    doc_content = defaultdict(lambda: defaultdict(dict))
    with mock.patch.object(documentation.Path, "exists", return_value=False):
        with mock.patch.object(
            documentation.Path, "write_bytes", autospec=True
        ) as mocked_writer:
            generate_docs(connectors=__CONNECTORS__)
            for call in mocked_writer.call_args_list:
                args, _ = call
                path, data = args
                if path.name == "README.md":
                    connector = path.parts[-2]
                    doc_digest[connector]["readme"] = hashlib.md5(data).hexdigest()
                    doc_content[connector]["readme"] = data.decode()
                else:
                    connector = path.parts[-3]
                    action = path.parts[-1].strip(".md")
                    doc_digest[connector]["actions"][action] = hashlib.md5(
                        data
                    ).hexdigest()
                    doc_content[connector]["actions"][action] = data.decode()
    return doc_digest, doc_content


@nox.session(python=PYTHON_VERSIONS)
def docs(session):
    baseline_doc_digest, baseline_content = generate_doc_digest()

    with requirements_file(session) as requirements:
        session.install("-r", requirements.name)

    with tempfile.NamedTemporaryFile("wb") as generated_digest_fp:
        with tempfile.NamedTemporaryFile("wb") as generated_content_fp:
            with tempfile.NamedTemporaryFile("wt") as script_fp:
                script_content = GENERATE_DOCUMENTATION.format(
                    GENERATE_DIGEST_SOURCE=inspect.getsource(generate_doc_digest),
                    GENARATE_DIGEST_FUNC_NAME=generate_doc_digest.__name__,
                    save_digest_to=generated_digest_fp.name,
                    save_content_to=generated_content_fp.name,
                )
                script_fp.write(script_content)
                script_fp.flush()
                session.run(
                    "python",
                    script_fp.name,
                    env={
                        "PYTHONPATH": "./src/",
                    },
                )
            with open(generated_digest_fp.name, "rt") as generated_digest:
                generated_digest = json.loads(generated_digest.read())
            with open(generated_content_fp.name, "rt") as generated_content:
                generated_content = json.loads(generated_content.read())
    if generated_digest != baseline_doc_digest:
        connectors_directory = Path("./src/hrflow_connectors/connectors")
        difference = []
        for connector, digests in baseline_doc_digest.items():
            if digests["readme"] != generated_digest.get(connector, {}).get("readme"):
                file = str(connectors_directory / connector / "README.md")
                baseline = baseline_content[connector]["readme"]
                generated = generated_content.get(connector, {}).get("readme", "")
                difference.append(
                    difflib.unified_diff(
                        a=baseline.splitlines(keepends=True),
                        b=generated.splitlines(keepends=True),
                        fromfile=file,
                        tofile=session.python,
                    )
                )
            for action, digest in digests["actions"].items():
                if digest != generated_digest.get(connector, {}).get("actions", {}).get(
                    action
                ):
                    file = str(
                        connectors_directory / connector / "docs" / f"{action}.md"
                    )
                    baseline = baseline_content[connector]["actions"][action]
                    generated = (
                        generated_content.get(connector, {})
                        .get("actions", {})
                        .get(action, "")
                    )
                    difference.append(
                        difflib.unified_diff(
                            a=baseline.splitlines(keepends=True),
                            b=generated.splitlines(keepends=True),
                            fromfile=file,
                            tofile=session.python,
                        )
                    )
        with tempfile.NamedTemporaryFile(
            "wt", suffix=".diff", delete=False
        ) as difffile:
            for diff in difference:
                difffile.write("\n")
                difffile.writelines(diff)
        session.error(
            f"Documentation python{session.python} is "
            "different than reference\n"
            f"Check difference here {difffile.name}"
        )
