# This code originally came from Legacy-master branch
# https://github.com/Riminder/hrflow-connectors/blob/legacy-master-ts-1663850377/src/hrflow_connectors/utils/adress_to_lat_long.py
# and was directly copied into the current branch without any changes.
import pkgutil
import re

import requests

# French Departements File
FRENCH_DEPARTMENTS_FILE = "../data/french_departement_geo_mapping.csv"
# French Cities File
FRENCH_CITIES_FILE = "../data/french_citycode_geo_mapping.csv"
# French Cities File
FRENCH_CITIES_NAME_FILE = "../data/french_cityname_geo_mapping.csv"


def accent_replacer(s: str) -> str:
    """
    Replace the accentuated characters by same character without accent.

    Args:
        s: the string we want to remove accentuated letters.

    Returns:
        The unaccentuated string.
    """
    s = s.replace("é", "e")
    s = s.replace("è", "e")
    s = s.replace("ê", "e")
    s = s.replace("ë", "e")
    s = s.replace("à", "a")
    s = s.replace("â", "a")
    s = s.replace("ä", "a")
    s = s.replace("ï", "i")
    s = s.replace("î", "i")
    s = s.replace("ù", "u")
    s = s.replace("û", "u")
    s = s.replace("ü", "u")
    s = s.replace("ç", "c")
    return s


def get_cities_code_lat_long_mapping() -> dict:
    """
    Get the dict of all postcode of french cities.

    Returns:
        exemple : ville_code = {"47380": ("44.525200649300004", "0.476034019359")}
    """
    ville_code = dict()
    # name used for the print.
    ville_code["name"] = "cities_codes"
    data = pkgutil.get_data(__name__, "../data/french_citycode_geo_mapping.csv")
    str_data = data.decode("utf-8")
    for line in str_data.split("\n"):
        row = line.split(",")
        code_str = str(row[0])
        # fill the uncompleted zip codes with zeros. (1 400 => 01 400)
        code = code_str.zfill(5)
        ville_code[code] = (row[1], row[2])

    return ville_code


def get_cities_names_lat_long_mapping() -> dict:
    """
    Get the dict of all names of french cities.

    Returns:
        exemple : ville_name = {"bergerac": ("44.854375187200006", "0.486529423457")}
    """
    ville_name = dict()
    # name used for the print.
    ville_name["name"] = "cities_name"
    data = pkgutil.get_data(__name__, "../data/french_cityname_geo_mapping.csv")
    str_data = data.decode("utf-8")
    for line in str_data.split("\n"):
        # .lower() for case sensitivity and accent_replacer(s) for accents.
        row = line.split(",")
        ville_name[accent_replacer(row[0].lower())] = (row[1], row[2])

    return ville_name


def get_departments_codes_lat_long_mapping() -> dict:
    """
    Get the dict of all postcode of french departments.

    Returns:
        exemple : departement = {"47": ("44.34453", "0.94193")}
    """
    departement = dict()
    # name used for the print.
    departement["name"] = "departments_codes"
    data = pkgutil.get_data(__name__, "../data/french_departement_geo_mapping.csv")
    str_data = data.decode("utf-8")
    for line in str_data.split("\n"):
        row = line.split(",")
        departement[row[0]] = (row[1], row[2])

    return departement


# Constant Dicts to avoid loading the files each time.
CITIES_CODES_DICT = get_cities_code_lat_long_mapping()
CITIES_NAMES_DICT = get_cities_names_lat_long_mapping()
DEPARTMENTS_CODES_DICT = get_departments_codes_lat_long_mapping()


def get_geolocation_data(
    location: str,
    cities_codes_dict: dict = None,
    cities_names_dict: dict = None,
    departments_codes_dict: dict = None,
    api_key=None,
):
    """
    Get the tuple latitude, longitude of a location with a 4 level of fall back.
        level 1: cities's postcode.
        level 2: cities's name.
        level 3: departments postcode.
        level 4: call here API.

    Args:
        location: the string of the location we want to get latitude and longitude.
        api_key: Here's api_key permitting the last fall back.
        cities_codes_dict:
        cities_names_dict:
        departments_codes_dict:

    Returns:
        latitude, longitude of the location.
    """
    cities_codes_dict = cities_codes_dict or CITIES_CODES_DICT
    cities_names_dict = cities_names_dict or CITIES_NAMES_DICT
    departments_codes_dict = departments_codes_dict or DEPARTMENTS_CODES_DICT

    fall_back_list = [cities_codes_dict, cities_names_dict, departments_codes_dict]
    for fall_back in fall_back_list:
        # get each word of the location.
        words_list = re.split("[,;._()/ ]+", accent_replacer(location))
        for word in words_list:
            value = fall_back.get(word.lower())
            if value:
                name = fall_back["name"]
                lat, long = value
                return name, lat, long

    if api_key is None:
        return None, None, None

    # If provided api_key, call here API to decode.
    URL = "https://geocode.search.hereapi.com/v1/geocode"
    PARAMS = {"apikey": api_key, "q": location}
    # sending get request and saving the response as response object
    r = requests.get(url=URL, params=PARAMS)
    data = r.json()
    if data.get("items"):
        # Acquiring the latitude and longitude from JSON
        latitude = data["items"][0]["position"]["lat"]
        longitude = data["items"][0]["position"]["lng"]
        return "here", latitude, longitude
    return None, None, None
