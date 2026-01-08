# Count SSB metadata tags

Count the number of times a tag has been used.

## Usage

``` r
count_ssb_tags(metadata)
```

## Arguments

- metadata:

  A data frame with SSB metadata.

## Value

data.frame

## Examples

``` r
# Get the metadata
meta <- get_ssb_metadata()
# Count tags
tags <- count_ssb_tags(meta)
```
