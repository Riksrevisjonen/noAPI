# Changelog

## noAPI 0.1.1 (October 4 2024)

### Enhancements

- Add functionality to retrieve sub-entities from the *Central
  Coordinating Register for Legal Entities* to
  [`get_entity()`](https://riksrevisjonen.github.io/noAPI/reference/get_entity.md)
  ([\#47](https://github.com/Riksrevisjonen/noAPI/issues/47))
- Add municipality number to the response from
  [`get_entity()`](https://riksrevisjonen.github.io/noAPI/reference/get_entity.md)

### New features

- Add new function
  [`get_schools()`](https://riksrevisjonen.github.io/noAPI/reference/get_schools.md)
  ([\#33](https://github.com/Riksrevisjonen/noAPI/issues/33))
- Add new function
  [`get_kindergartens()`](https://riksrevisjonen.github.io/noAPI/reference/get_kindergartens.md)
  ([\#34](https://github.com/Riksrevisjonen/noAPI/issues/34))
- Add functionality to fetch data from Statistics Norway’s ready-made
  datasets ([\#56](https://github.com/Riksrevisjonen/noAPI/issues/56))

### Bug fixes

- Improve warning message for
  [`get_entity()`](https://riksrevisjonen.github.io/noAPI/reference/get_entity.md)
  when a name query fails
  ([\#51](https://github.com/Riksrevisjonen/noAPI/issues/51))

## noAPI 0.1.0 (March 29 2023)

This is the first release of noAPI, introducing the following functions:

Brønnøysundsregisteret:

- [`get_entity()`](https://riksrevisjonen.github.io/noAPI/reference/get_entity.md)
- [`get_roles()`](https://riksrevisjonen.github.io/noAPI/reference/get_roles.md)

Kartverket:

- [`get_address_info()`](https://riksrevisjonen.github.io/noAPI/reference/get_address_info.md)
- [`find_address_from_point()`](https://riksrevisjonen.github.io/noAPI/reference/find_address_from_point.md)

SSB:

- [`get_adm_units()`](https://riksrevisjonen.github.io/noAPI/reference/get_municipalities.md)
- [`get_municipalities()`](https://riksrevisjonen.github.io/noAPI/reference/get_municipalities.md)
- [`get_counties()`](https://riksrevisjonen.github.io/noAPI/reference/get_municipalities.md)
- [`get_countries()`](https://riksrevisjonen.github.io/noAPI/reference/get_countries.md)
- [`get_industrial_codes()`](https://riksrevisjonen.github.io/noAPI/reference/get_industrial_codes.md)

Norges Bank:

- [`get_exchange_rate()`](https://riksrevisjonen.github.io/noAPI/reference/get_exchange_rate.md)
