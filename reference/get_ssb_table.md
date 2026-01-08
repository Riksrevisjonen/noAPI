# Get SSB table

Fetch ready-made tables from Statistics Norway (SSB).

## Usage

``` r
get_ssb_table(id)
```

## Arguments

- id:

  The table ID. Corresponds to the ID variable in the metadata.

## Value

data.frame

## Details

The function returns a ready-made table produced by Statistics Norway,
based on the provided table ID. A list of all available IDs can be found
with
[`get_ssb_metadata()`](https://riksrevisjonen.github.io/noAPI/reference/get_ssb_metadata.md).

Note that the metadata is updated daily at 05.00 and 11.30 AM, which can
lead to the API being unavailable for up to five minutes.

See the [API documentation](https://data.ssb.no/api/) for further
details.

## See also

[`get_ssb_metadata()`](https://riksrevisjonen.github.io/noAPI/reference/get_ssb_metadata.md)
to view the metadata.

## Examples

``` r
# Get table with ID '1104'
df <- get_ssb_table('1104')
```
