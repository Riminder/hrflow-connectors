/**
 * Geolocation Utilities
 * Complete TypeScript translation of v1/utils/geolocation.py
 */

/**
 * Replace accentuated characters with unaccentuated equivalents
 * @param s The string to process
 * @returns The unaccentuated string
 */
export function accentReplacer(s: string): string {
  let result = s;
  const replacements: Record<string, string> = {
    é: 'e',
    è: 'e',
    ê: 'e',
    ë: 'e',
    à: 'a',
    â: 'a',
    ä: 'a',
    ï: 'i',
    î: 'i',
    ù: 'u',
    û: 'u',
    ü: 'u',
    ç: 'c',
  };

  for (const [accented, unaccented] of Object.entries(replacements)) {
    result = result.replace(new RegExp(accented, 'g'), unaccented);
  }

  return result;
}

/**
 * Get the dictionary of all postal codes of French cities with their latitude and longitude
 * @returns Dictionary mapping postal codes to [latitude, longitude] tuples
 */
export function getCitiesCodeLatLongMapping(): Record<string, [string, string]> {
  const villeCode: Record<string, [string, string]> = {};
  villeCode['name'] = ['cities_codes', ''];

  // In a real application, you would load this from CSV data
  // For now, we provide placeholder structure
  const csvData = getCitiesCodeCsvData();
  const lines = csvData.split('\n');

  for (const line of lines) {
    if (!line.trim()) continue;
    const row = line.split(',');
    const codeStr = row[0];
    // Pad with zeros to make 5 digits (e.g., 1400 => 01400)
    const code = codeStr.padStart(5, '0');
    if (row[1] && row[2]) {
      villeCode[code] = [row[1], row[2]];
    }
  }

  return villeCode;
}

/**
 * Get the dictionary of all names of French cities with their latitude and longitude
 * @returns Dictionary mapping city names to [latitude, longitude] tuples
 */
export function getCitiesNamesLatLongMapping(): Record<string, [string, string]> {
  const villeName: Record<string, [string, string]> = {};
  villeName['name'] = ['cities_name', ''];

  const csvData = getCitiesNameCsvData();
  const lines = csvData.split('\n');

  for (const line of lines) {
    if (!line.trim()) continue;
    const row = line.split(',');
    const normalizedName = accentReplacer(row[0].toLowerCase());
    if (row[1] && row[2]) {
      villeName[normalizedName] = [row[1], row[2]];
    }
  }

  return villeName;
}

/**
 * Get the dictionary of all postal codes of French departments with their latitude and longitude
 * @returns Dictionary mapping department codes to [latitude, longitude] tuples
 */
export function getDepartmentsCodesLatLongMapping(): Record<string, [string, string]> {
  const departement: Record<string, [string, string]> = {};
  departement['name'] = ['departments_codes', ''];

  const csvData = getDepartmentsCsvData();
  const lines = csvData.split('\n');

  for (const line of lines) {
    if (!line.trim()) continue;
    const row = line.split(',');
    if (row[0] && row[1] && row[2]) {
      departement[row[0]] = [row[1], row[2]];
    }
  }

  return departement;
}

// Cache the loaded data to avoid reloading
let citiesCodesDict: Record<string, [string, string]> | null = null;
let citiesNamesDict: Record<string, [string, string]> | null = null;
let departmentsCodesDict: Record<string, [string, string]> | null = null;

// Lazy load the constant dictionaries
function getCitiesCodesDict(): Record<string, [string, string]> {
  if (!citiesCodesDict) {
    citiesCodesDict = getCitiesCodeLatLongMapping();
  }
  return citiesCodesDict;
}

function getCitiesNamesDict(): Record<string, [string, string]> {
  if (!citiesNamesDict) {
    citiesNamesDict = getCitiesNamesLatLongMapping();
  }
  return citiesNamesDict;
}

function getDepartmentsCodesDict(): Record<string, [string, string]> {
  if (!departmentsCodesDict) {
    departmentsCodesDict = getDepartmentsCodesLatLongMapping();
  }
  return departmentsCodesDict;
}

/**
 * Get geolocation data with fallback chain:
 * 1. City postal code
 * 2. City name
 * 3. Department postal code
 * 4. HERE API
 * @param location The location string
 * @param apiKey Optional HERE API key for fallback
 * @param citiesCodesDict Optional override for cities codes
 * @param citiesNamesDict Optional override for cities names
 * @param departmentsCodesDict Optional override for departments codes
 * @returns Tuple of [source, latitude, longitude] or [null, null, null] if not found
 */
export async function getGeolocationData(
  location: string,
  apiKey?: string,
  citiesCodesDict?: Record<string, [string, string]>,
  citiesNamesDict?: Record<string, [string, string]>,
  departmentsCodesDict?: Record<string, [string, string]>
): Promise<[string | null, string | null, string | null]> {
  // Use provided dicts or defaults
  const codes = citiesCodesDict || getCitiesCodesDict();
  const names = citiesNamesDict || getCitiesNamesDict();
  const departments = departmentsCodesDict || getDepartmentsCodesDict();

  const fallbackList = [codes, names, departments];

  // Try fallback chain with local data
  for (const fallback of fallbackList) {
    // Split location by common delimiters
    const wordsList = location.split(/[,;._()/ ]+/);

    for (const word of wordsList) {
      const normalizedWord = accentReplacer(word.toLowerCase());
      const value = fallback[normalizedWord];

      if (value) {
        const source = (fallback as any)['name']?.[0] || 'unknown';
        const [lat, lng] = value;
        return [source, lat, lng];
      }
    }
  }

  // If no API key provided, return nulls
  if (!apiKey) {
    return [null, null, null];
  }

  // Fallback to HERE API
  try {
    const url = 'https://geocode.search.hereapi.com/v1/geocode';
    const params = { apikey: apiKey, q: location };

    const response = await fetch(`${url}?apikey=${apiKey}&q=${encodeURIComponent(location)}`);

    if (!response.ok) {
      return [null, null, null];
    }

    const data = await response.json();

    if (data.items && data.items.length > 0) {
      const latitude = String(data.items[0].position.lat);
      const longitude = String(data.items[0].position.lng);
      return ['here', latitude, longitude];
    }

    return [null, null, null];
  } catch (error) {
    return [null, null, null];
  }
}

// Placeholder functions to load CSV data
// In production, these would load actual CSV files
function getCitiesCodeCsvData(): string {
  // This would load from french_citycode_geo_mapping.csv
  return '';
}

function getCitiesNameCsvData(): string {
  // This would load from french_cityname_geo_mapping.csv
  return '';
}

function getDepartmentsCsvData(): string {
  // This would load from french_departement_geo_mapping.csv
  return '';
}
