# Query parameters for Find Address

Accepted query parameters for Kartverket's endpoint `/punktsok`.

## Usage

``` r
create_params_kv_punktsok(
  lat,
  lon,
  radius,
  crs,
  ascii_compatible = TRUE,
  size = 1000,
  page = 0
)
```

## Arguments

- radius:

  An integer value with the radius for the query, in whole meters.

- crs:

  An integer or vector with the input and output coordinate reference
  system. Default value is ETRS89 for latitude and longitude data
  (EPSG:4258).

- ascii_compatible:

  If TRUE (default) the returned data is ASCII compatible.

- size:

  An integer value with the size for each page. Defaults to 1000.

- page:

  An integer value with the page to query. Defaults to 0 (the first
  page).
