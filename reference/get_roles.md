# Get roles

Fetch information about entity roles from the *Central Coordinating
Register for Legal Entities* (Enhetsregisteret) based on the
organisation number.

## Usage

``` r
get_roles(entity, simplify = TRUE, raw_response = FALSE)
```

## Arguments

- entity:

  A Norwegian organisation number.

- simplify:

  If `TRUE` (default), a single data.frame is returned. Ignored if
  `raw_response` is set to `TRUE`.

- raw_response:

  If `TRUE` a list of class `noAPI` is returned, including the raw
  `httr2_response`.

## Value

list

## Details

The function returns a data.frame by default. If you prefer the output
as a list you can set `simplify` to `FALSE`. This can be useful to keep
programmatically track of failed queries. If you set `raw_response` to
`TRUE`, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

See the [API
documentation](https://data.brreg.no/enhetsregisteret/api/docs/index.html)
for further details (in Norwegian only).

## See also

[`get_entity()`](https://riksrevisjonen.github.io/noAPI/reference/get_entity.md).
It's not possible to get roles for an entity using a name search. If you
do not have an organisation number, you should first do a call to
[`get_entity()`](https://riksrevisjonen.github.io/noAPI/reference/get_entity.md)
and then call `get_roles()` using the response from the first call.

## Examples

``` r
# Get roles for a single entity
df <- get_roles(974760843)

# Get roles for a mulitiple enities
df <- get_roles(c(974760843, 971524960))
```
