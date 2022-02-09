from jinja2 import Template

CONNECTOR_README_TEMPLATE = Template(
    """
# {{ connector_name }} Connector
{% if description %}
> {{ description }}
{% endif %}

🔗 {{ url }}

| Actions |
| ------- |
{% for action in actions %}| [**{{ action.name | title | replace("_", " ")}}**](docs/{{ action.name }}.md) |
{% endfor %}
"""
)
ACTION_DOCUMENTATION_TEMAPLTE = Template(
    """
# {{ action_name | title | replace("_", " ") }}
`{{ source_name }}` :arrow_right: `{{ destination_name }}`

{{ description }}

{% if source_endpoints %}
**{{ source_name }} endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
{%- for endpoint in source_endpoints %}
| [**{{ endpoint.name}}**]({{ endpoint.url }}) | {{ endpoint.description }} |
{%- endfor %}

{% endif %}
{% if destination_endpoints %}
**{{ destination_name }} endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
{%- for endpoint in destination_endpoints %}
| [**{{ endpoint.name}}**]({{ endpoint.url }}) | {{ endpoint.description }} |
{%- endfor %}

{% endif %}
## Action Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
{%- for field in action_fields %}
| `{{ field.name }}` {% if field.required %}:red_circle:{% endif %} | `{{ field.type }}` | {{ field.default }} | {{ field.description }} |
{%- endfor %}

## Source Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
{%- for field in source_fields %}
| `{{ field.name }}` {% if field.required %}:red_circle:{% endif %} | `{{ field.type }}` | {{ field.default }} | {{ field.description }} |
{%- endfor %}

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
{%- for field in destination_fields %}
| `{{ field.name }}` {% if field.required %}:red_circle:{% endif %} | `{{ field.type }}` | {{ field.default }} | {{ field.description }} |
{%- endfor %}

:red_circle: : *required*

## Example

```python
import logging
from hrflow_connectors import {{ connector_name }}


logging.basicConfig(level=logging.INFO)


{{ connector_name }}.{{ action_name }}(
    action_parameters=dict(
        {%- for field in action_fields %}
        {{ field.name }}={{ field.example }},
        {%- endfor %}
    ),
    source_parameters=dict(
        {%- for field in source_fields %}
        {{ field.name }}={{ field.example }},
        {%- endfor %}
    ),
    destination_parameters=dict(
        {%- for field in destination_fields %}
        {{ field.name }}={{ field.example }},
        {%- endfor %}
    )
)
```
"""
)

WORKFLOW_TEMPLATE = Template(
    """
import typing as t

from hrflow_connectors import {{ connector_name }}

{{ format_placeholder }}

{{ logics_placeholder }}

def workflow(
        {% if type == "catch" %}
        _request: t.Dict,
        {% endif %}
        settings: t.Dict
    ) -> None:
    actions_parameters = dict()
    try:
        format
    except NameError:
        pass
    else:
        actions_parameters["format"] = format

    try:
        logics
    except NameError:
        pass
    else:
        actions_parameters["logics"] = logics

    {% if type == "pull" %}
    pull_parameters_from = settings
    {% else %}
    pull_parameters_from = {**settings, **_request}
    {% endif %}

    {{ connector_name }}.{{ action_name }}(
        action_parameters=actions_parameters,
        source_parameters=dict(
            {%- for parameter in source_parameters %}
                {{ parameter }}=pull_parameters_from.get("{{ parameter }}"),
            {%- endfor %}
        ),
        destination_parameters=dict(
            {%- for parameter in destination_parameters %}
                {{ parameter }}=pull_parameters_from.get("{{ parameter }}"),
            {%- endfor %}
        ),
    )
"""
)
