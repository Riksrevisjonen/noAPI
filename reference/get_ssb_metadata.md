# Get SSB table metadata

Fetch metadata for ready-made tables from Statistics Norway (SSB).

## Usage

``` r
get_ssb_metadata(data_tag = NULL)
```

## Arguments

- data_tag:

  Optional. Data tag to filter on.

## Value

data.frame

## Details

The function returns an overview of all the available ready-made tables
produced by Statistics Norway.

Note that the metadata is updated daily at 05.00 and 11.30 AM, which can
lead to the API being unavailable for up to five minutes.

See the [API documentation](https://data.ssb.no/api/) for further
details.

## See also

[`get_ssb_table()`](https://riksrevisjonen.github.io/noAPI/reference/get_ssb_table.md)
to retrieve the actual tables.

## Examples

``` r
# Get all metadata
ssb_metadata <- get_ssb_metadata()

# Get a subset containing certain tags
ssb_metadata_arbeidskraft <- get_ssb_metadata(data_tag = 'arbeidskraft')
```
