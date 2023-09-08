from jinja2 import Environment, PackageLoader

Templates = Environment(
    loader=PackageLoader(
        package_name="hrflow_connectors",
        package_path="core/templates",
    ),
)
