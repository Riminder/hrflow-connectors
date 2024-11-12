from jinja2 import Environment, PackageLoader

Templates = Environment(
    loader=PackageLoader(
        package_name="hrflow_connectors",
        package_path="v2/core/templates",
    ),
)
