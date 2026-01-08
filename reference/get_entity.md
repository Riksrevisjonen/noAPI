# Get entity

Fetch information about an entity from the *Central Coordinating
Register for Legal Entities* (Enhetsregisteret) based on the entity's
name or organisation number.

## Usage

``` r
get_entity(entity, type = NULL, simplify = TRUE, raw_response = FALSE)
```

## Arguments

- entity:

  A Norwegian organisation number or company name.

- type:

  Optional. Type entity to query. Either main, sub or NULL (default).
  See details.

- simplify:

  If `TRUE` (default), a single data.frame is returned. Ignored if
  `raw_response` is set to `TRUE`.

- raw_response:

  If `TRUE` a list of class `noAPI` is returned, including the raw
  `httr2_response`.

## Value

data.frame or list

## Details

Users should be aware that queries for entity names and numbers are
handled somewhat differently. By default (`type = NULL`) queries for
*numbers* will first send requests to the registry for *main entities*,
but will then also query the registry for *sub-entities* in case no
information is found in the first query. This ensures that any entity
number still in use will return a valid response. For *names* however
`get_entity()` will only query the registry for *main entities*, unless
`type` is explicitly set to `sub`. Further information on the use of
sub-entities can be found
[here](https://www.brreg.no/bedrift/underenhet/).

The function returns a data.frame by default. If you prefer the output
as a list you can set `simplify` to `FALSE`. This can be useful to keep
programmatically track of failed queries. If you set `raw_response` to
`TRUE`, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

See the [API
documentation](https://data.brreg.no/enhetsregisteret/api/docs/index.html)
for further details (in Norwegian only).

## Examples

``` r
# Get entity by number
df <- get_entity(974760843)

# Get entity by name
df <- get_entity('Riksrevisjonen')

# Get multiple entities
df <- get_entity(c(974760843, 971524960))

# Get information on sub-entities (number)
df <- get_entity(999178197)

#' # Get information on sub-entities (name)
df <- get_entity('Kuben Yrkesarena', type = 'sub')
```
