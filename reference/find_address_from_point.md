# Find addresses within a geographical area

Search for Norwegian addresses within a certain radius from a
geographical point.

## Usage

``` r
find_address_from_point(
  x,
  radius = 50,
  crs = 4258,
  closest = FALSE,
  ...,
  simplify = TRUE,
  raw_response = FALSE
)
```

## Arguments

- x:

  A vector or list with latitude and longitude coordinates.

- radius:

  An integer value with the radius for the query, in whole meters.

- crs:

  An integer or vector with the input and output coordinate reference
  system. Default value is ETRS89 for latitude and longitude data
  (EPSG:4258).

- closest:

  If TRUE only the address that is closest to the geographical point is
  returned.

- ...:

  Additional arguments passed to the API call.

- simplify:

  If `TRUE` (default), a single data.frame is returned. Ignored if
  `raw_response` is set to `TRUE`.

- raw_response:

  If `TRUE` a list of class `noAPI` is returned, including the raw
  `httr2_response`.

## Value

data.frame or list

## Details

For most use cases ETRS89 (EPSG:4258) and WSG84 (EPSG:4326) can be
considered equal, but users should be aware that there is a slight shift
on both axes between the two coordinate systems. If high precision is
needed `crs` should be set to 4326.

Advanced users might want to look into the additional possible
parameters. For a list of all accepted query parameters see
[`create_params_kv_punktsok()`](https://riksrevisjonen.github.io/noAPI/reference/create_params_kv_punktsok.md)
or refer to the [API
documentation](https://ws.geonorge.no/adresser/v1/#/default/get_sok) for
further details (in Norwegian only).

The function returns a data.frame by default. If you prefer the output
as a list you can set `simplify` to `FALSE`. This can be useful to keep
programmatically track of failed queries. If you set `raw_response` to
`TRUE`, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

## Examples

``` r
# Using a vector
df <- find_address_from_point(c(lat = 63.42805, lon = 10.39679))

# Using a list
dl <- find_address_from_point(
  list(c(lat = 59.91364, lon = 10.7508),
       c(lat = 63.42805, lon = 10.39679)))
```
