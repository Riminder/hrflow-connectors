import os
from hrflow_connectors.utils.config import ConfigError

def test_Config_same_var_in_env_file(pytestconfig, config):
    # Read `.env` file
    env_path = os.path.join(pytestconfig.rootpath, ".env")
    with open(env_path, "r") as env_file:
        env_line_list = env_file.readlines()

    def is_hrflow_connectors_var(line):
        return line.startswith("HRFLOW_CONNECTORS_")

    def clean_line(line):
        return line.strip()

    def remove_prefix(line):
        prefix_last_char_position = len("HRFLOW_CONNECTORS_")
        equal_char_position = line.index("=")
        var_name = line[prefix_last_char_position:equal_char_position]
        return var_name.strip()

    cleaned_line_list = map(clean_line, env_line_list)
    filtered_line_list = filter(is_hrflow_connectors_var, cleaned_line_list)
    var_list_expected = map(remove_prefix, filtered_line_list)
    var_list_expected = list(var_list_expected)
    var_list_expected.sort()

    # Get var list in Config
    var_list_got = [var_name for var_name in config.__dict__]
    var_list_got = list(var_list_got)
    var_list_got.sort()

    # Check var list
    assert var_list_expected == var_list_got


def test_Config_check_each_value(config):
    # Get all environment variables
    def is_hrflow_connectors_var(line):
        return line.startswith("HRFLOW_CONNECTORS_")

    def remove_prefix(line):
        prefix_last_char_position = len("HRFLOW_CONNECTORS_")
        return line[prefix_last_char_position:]

    environement_var_list = [var_name for var_name in os.environ]
    hrflow_connectors_var_list_expected = filter(
        is_hrflow_connectors_var, environement_var_list
    )
    hrflow_connectors_var_list_expected = list(hrflow_connectors_var_list_expected)
    
    hrflow_connectors_var_expected = dict()
    for var_name in hrflow_connectors_var_list_expected:
        var_name_without_prefix = remove_prefix(var_name)
        hrflow_connectors_var_expected[var_name_without_prefix] = os.environ[var_name]

    # Get var in Config
    hrflow_connectors_var_got = config.__dict__

    # Check var
    assert hrflow_connectors_var_expected == hrflow_connectors_var_got

def test_Config_field_not_found(config):
    try:
        config.THIS_FIELD_DOES_NOT_EXIST
        assert False
    except ConfigError:
        pass