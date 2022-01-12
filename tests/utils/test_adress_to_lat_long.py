from hrflow_connectors.utils.adress_to_lat_long import get_departments_codes_lat_long_mapping
from hrflow_connectors.utils.adress_to_lat_long import get_cities_names_lat_long_mapping
from hrflow_connectors.utils.adress_to_lat_long import get_cities_code_lat_long_mapping
from hrflow_connectors.utils.adress_to_lat_long import get_lat_lng


def test_departments_codes_lat_long_mapping():
    lat_long_map = get_departments_codes_lat_long_mapping()
    assert lat_long_map["name"] == "departments_codes"
    assert lat_long_map["01"] == ('46.26462', '5.65979')
    assert lat_long_map["05"] == ('44.55986', '6.08057')
    assert lat_long_map["16"] == ('45.7026', '-0.31493')
    assert lat_long_map["24"] == ('44.84587', '0.49812')
    assert lat_long_map["31"] == ('43.6211', '1.41814')
    assert lat_long_map["54"] == ('48.88522', '6.04267')
    assert lat_long_map["60"] == ('49.46644', '2.11035')
    assert lat_long_map["76"] == ('49.49541', '0.11531')
    assert lat_long_map["972"] == ('49.25382', '2.46689')
    assert lat_long_map["974"] == ('44.29685', '0.11835')


def test_cities_names_lat_long_mapping():
    lat_long_map = get_cities_names_lat_long_mapping()
    assert lat_long_map["name"] == "cities_name"
    assert lat_long_map["abergement-clémenciat"] == ('46.1534255214', '4.92611354223')
    assert lat_long_map["broué"] == ('48.7523989393', '1.52435606759')
    assert lat_long_map["tombebœuf"] == ('44.5192333821', '0.45231261712399995')
    assert lat_long_map["bergerac"] == ('44.854375187200006', '0.486529423457')
    assert lat_long_map["hautefaye"] == ('45.5360605721', '0.508242554831')
    assert lat_long_map["villetoureix"] == ('45.2729009279', '0.359083456128')
    assert lat_long_map["beure"] == ('47.2060766708', '6.00526129105')
    assert lat_long_map["saint-didier-au-mont-d'or"] == ('45.8147288136', '4.79844330211')
    assert lat_long_map["courchaton"] == ('47.5135728271', '6.5542014435')
    assert lat_long_map["rungis"] == ('48.7494821504', '2.35281832827')


def test_cities_code_lat_long_mapping():
    lat_long_map = get_cities_code_lat_long_mapping()
    assert lat_long_map["name"] == "cities_codes"
    assert lat_long_map["01400"] == ('46.1735383313', '4.9577238252599996')
    assert lat_long_map["47380"] == ('44.525200649300004', '0.476034019359')
    assert lat_long_map["24100"] == ('44.82370898600001', '0.44590400456699997')
    assert lat_long_map["45470"] == ('47.9652949917', '2.10398859914')
    assert lat_long_map["54780"] == ('49.173261325', '5.91605842347')
    assert lat_long_map["59230"] == ('50.446729768000004', '3.33012536259')
    assert lat_long_map["63970"] == ('45.6309455318', '2.89220399205')
    assert lat_long_map["72370"] == ('47.979285909', '0.46251459500799996')
    assert lat_long_map["97425"] == ('-21.208999429400002', '55.359148544700005')
    assert lat_long_map["98761"] == ('nan', 'nan')
