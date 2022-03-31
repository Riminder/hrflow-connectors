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
`{{ origin_name }}` :arrow_right: `{{ target_name }}`

{{ description }}

{% if origin_endpoints %}
**{{ origin_name }} endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
{%- for endpoint in origin_endpoints %}
| [**{{ endpoint.name}}**]({{ endpoint.url }}) | {{ endpoint.description }} |
{%- endfor %}

{% endif %}
{% if target_endpoints %}
**{{ target_name }} endpoints used :**
| Endpoints | Description |
| --------- | ----------- |
{%- for endpoint in target_endpoints %}
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
{%- for field in origin_fields %}
| `{{ field.name }}` {% if field.required %}:red_circle:{% endif %} | `{{ field.type }}` | {{ field.default }} | {{ field.description }} |
{%- endfor %}

## Destination Parameters

| Field | Type | Default | Description |
| ----- | ---- | ------- | ----------- |
{%- for field in target_fields %}
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
    origin_parameters=dict(
        {%- for field in origin_fields %}
        {{ field.name }}={{ field.example }},
        {%- endfor %}
    ),
    target_parameters=dict(
        {%- for field in target_fields %}
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

ORIGIN_SETTINGS_PREFIX = "{{ origin_settings_prefix }}"
TARGET_SETTINGS_PREFIX = "{{ target_settings_prefix }}"

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
    parameters = settings
    {% else %}
    parameters = {**settings, **_request}
    {% endif %}

    {{ connector_name }}.{{ action_name }}(
        action_parameters=actions_parameters,
        origin_parameters=dict(
            {%- for parameter in origin_parameters %}
                {{ parameter }}=parameters.get("{}{{ parameter }}".format(ORIGIN_SETTINGS_PREFIX)),
            {%- endfor %}
        ),
        target_parameters=dict(
            {%- for parameter in target_parameters %}
                {{ parameter }}=parameters.get("{}{{ parameter }}".format(TARGET_SETTINGS_PREFIX)),
            {%- endfor %}
        ),
    )
"""
)
