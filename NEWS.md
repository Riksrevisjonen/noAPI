# noAPI 0.1.0.9000

## Enhancements

* Add functionality to retrieve sub-entities from the _Central Coordinating Register for Legal Entities_ to `get_entity()` (#47)

## New features

* Add new function `get_schools()` (#33)
* Add new function `get_kindergartens()` (#34)
* Add functionality to fetch data from Statistics Norway's ready-made datasets (#56)


# noAPI 0.1.0 (March 29 2023)

This is the first release of noAPI, introducing the following functions:

Brønnøysundsregisteret:

* `get_entity()`
* `get_roles()`

Kartverket:

* `get_address_info()`
* `find_address_from_point()`

SSB:

* `get_adm_units()`
* `get_municipalities()`
* `get_counties()`
* `get_countries()`
* `get_industrial_codes()`

Norges Bank:

* `get_exchange_rate()`

