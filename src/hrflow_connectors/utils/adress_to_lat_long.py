import requests
import pkgutil
import re


def accent_replacer(s):
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


def get_cities_code_lat_long_mapping():
    """
    Get the dict of all postcode of french cities.

    Returns:
        exemple : ville_code = {"47380": ("44.525200649300004", "0.476034019359")}
    """
    # retrieves the CSV containing the city codes.
    url = "https://drive.google.com/file/d/1E7URCGV1ShKhpz81axsOuP9fHorResXM/view?usp=sharing"
    path = "https://drive.google.com/uc?id=" + url.split("/")[-2]
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


def get_cities_names_lat_long_mapping():
    """
    Get the dict of all names of french cities.

    Returns:
        exemple : ville_name = {"bergerac": ("44.854375187200006", "0.486529423457")}
    """
    # retrieves the CSV containing the city names.
    url = "https://drive.google.com/file/d/17h0MgT-Y35fDUq04ggFoVj7WfY8h8pF6/view?usp=sharing"
    path = "https://drive.google.com/uc?id=" + url.split("/")[-2]
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


def get_departments_codes_lat_long_mapping():
    """
    Get the dict of all postcode of french departments.

    Returns:
        exemple : departement = {"47": ("44.34453", "0.94193")}
    """
    # retrieves the CSV containing the city names.
    url = "https://drive.google.com/file/d/1KeQxJlLkG650-KNnSgjAQ1yIJSGicwCW/view?usp=sharing"
    path = "https://drive.google.com/uc?id=" + url.split("/")[-2]
    departement = dict()
    # name used for the print.
    departement["name"] = "departments_codes"
    data = pkgutil.get_data(__name__, "../data/french_departement_geo_mapping.csv")
    str_data = data.decode("utf-8")
    for line in str_data.split("\n"):
        row = line.split(",")
        departement[row[0]] = (row[1], row[2])

    return departement


def get_lat_lng(
    location, cities_codes_dict, cities_names_dict, departments_codes_dict, api_key=None
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
