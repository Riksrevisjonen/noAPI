# Get Standard Industrial Classification (SIC)

Get all Standard Industrial Classification (SIC) based on the NACE
standard.

## Usage

``` r
get_industrial_codes(
  year = format(Sys.Date(), "%Y"),
  include_notes = FALSE,
  simplify = TRUE,
  raw_response = FALSE
)
```

## Arguments

- year:

  The year for which the codes should be valid for.

- include_notes:

  If notes should be included or not.

- simplify:

  If `TRUE` (default), a single data.frame is returned. Ignored if
  `raw_response` is set to `TRUE`.

- raw_response:

  If `TRUE` a list of class `noAPI` is returned, including the raw
  `httr2_response`.

## Value

data.frame or list

## Details

Standard Industrial Classification (SIC) is primarily a statistical
standard and is the basis for coding units according to the most
important activities in Statistics Norway's Business register and in the
Central Coordinating Register for Legal Entities. SIC2007 is based on
NACE Rev. 2. See [Statistics Norway's
webpage](https://www.ssb.no/klass/klassifikasjoner/6) for more
information on the code specification.

All years from 1994 until present are supported. The function defaults
to the current year. If notes are enabled with `include_notes`, a column
`note` will be added to the data.frame.

The function returns a data.frame by default. If you prefer the output
as a list you can set `simplify` to `FALSE`. This can be useful to keep
programmatically track of failed queries. If you set `raw_response` to
`TRUE`, the raw response from the API will be returned together with the
parsed response. Note that the response will then be returned silently.

## See also

[`get_entity()`](https://riksrevisjonen.github.io/noAPI/reference/get_entity.md)
