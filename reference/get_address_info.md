# Get address info

Search for Norwegian addresses in *The Norwegian Mapping Authority's
Cadastre and Land Registry*.

## Usage

``` r
get_address_info(
  search = NULL,
  crs = 4258,
  ...,
  simplify = TRUE,
  raw_response = FALSE
)
```

## Arguments

- search:

  A Norwegian address. For example: 'munkegata 1 trondheim'.

- crs:

  The desired coordinate reference system for the output. Default value
  is ETRS89 for latitude and longitude data (EPSG:4258). See details.

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
considered equal, but users should be aware hat there is a slight shift
on both axes between the two coordinate systems. If high precision is
needed `crs` should be set to 4326.

Many users will get by with using the regular `search` parameter, but
more advanced users might want to look into the wide range of possible
parameters. For a list of all accepted query parameters see
[`create_params_kv_sok()`](https://riksrevisjonen.github.io/noAPI/reference/create_params_kv_sok.md)
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
# A simple query
df <- get_address_info('munkegata 1 trondheim')

# Use the advanced parameters to be more specific
df <- get_address_info(address_name = 'Munkegata', address_number = 1,
         mun_name = 'Trondheim')

if (FALSE) { # \dontrun{
# Multiple adresses
df <- get_address_info(c('munkegata 1 trondheim', 'tromsø gate'))

# A more advanced query
df <- get_address_info(c('munkegata 1 trondheim', 'storgata 1'),
   mun_code = list(NULL, '0301'))

} # }
```
