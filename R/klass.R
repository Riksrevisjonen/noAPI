#' Get municipalities
#'
#' Get all Norwegian municipalities and municipality codes for a given year.
#'
#' Fetches all Norwegian municipalities and municipality codes for a given year.
#' All years from 1977 until present are supported. The function will default to
#' the current year. The function will return a data.frame with all municipalities.
#'
#' If more than one year is given, a single data.frame will be returned with all
#' codes and names for the given years, unless `simplify` is set to `FALSE`, then
#' a list of the same length of the input will be returned instead. If `raw_response`
#' is set to `TRUE`, a parsed data.frame will be returned along with each raw
#' response from the API. It is not possible to return a single data.frame when
#' `raw_response` is set to `TRUE`.
#'
#' You can add county numbers and names for each municipality by setting `add_county`
#' to `TRUE`.
#'
#' @inheritParams get_entity
#' @param year The year for which the codes should be valid for
#' @param add_county If set to `TRUE`, add county name and number
#' @param include_notes If notes should be included or not
#' @param simplify If `TRUE` (default), a single data.frame is returned.
#'
#' @return data.frame or list
#'
#' @seealso `get_counties()` for county codes and names
#'
#' @export
get_municipalities <- function(
    year = format(Sys.Date(), '%Y'), add_county = FALSE, include_notes = FALSE,
    simplify = TRUE, raw_response = FALSE)
{
  if (add_county) {
    if (raw_response) {
      raw_response <- FALSE
      cli::cli_warn('If add_county is set to `TRUE`, raw data cannot be returned')
    }
    x <- lapply(year, get_klass_codes_single, type = 'both',
                include_notes = include_notes, raw_response = raw_response)
  } else {
    x <- lapply(year, get_klass_codes_single, type = 'municipality',
                include_notes = include_notes, raw_response = raw_response)
  }
  if (!raw_response & simplify) {
    x <- do.call('rbind', x)
  }
  x
}

#' Get counties
#'
#' Get all Norwegian counties and county codes for a given year.
#'
#' Fetches all Norwegian counties and county codes for a given year. All years
#' from 1972 until present are supported. The function will default to
#' the current year. The function will return a data.frame with all counties.
#'
#' If more than one year is given, a single data.frame will be returned with all
#' codes and names for the given years, unless `simplify` is set to `FALSE`, then
#' a list of the same length of the input will be returned instead. If `raw_response`
#' is set to `TRUE`, a parsed data.frame will be returned along with each raw
#' response from the API. It is not possible to return a single data.frame when
#' `raw_response` is set to `TRUE`.
#'
#' @inheritParams get_municipalities
#'
#' @return data.frame or list
#'
#' @seealso `get_municipalities()` for municipality codes and names
#'
#' @export
get_counties <- function(
    year = format(Sys.Date(), '%Y'), include_notes = FALSE, simplify = TRUE,
    raw_response = FALSE)
{
  x <- lapply(year, get_klass_codes_single, type = 'county', include_notes = include_notes, raw_response = raw_response)
  if (!raw_response & simplify) {
    x <- do.call('rbind', x)
  }
  x
}

#' Get countries
#'
#' Get all country codes (alpha-3) based on the ISO 3166-1 standard, but adjusted
#' for data from Statistics Norway.
#'
#' Fetches all countries and country codes (alpha-3) for a given year based on the
#' ISO 3166-1 standard. The list includes special codes XUK and XXX for unknown
#' citizenship and stateless persons, respectively. These codes are used in official
#' statistics from Statistics Norway.
#'
#' All years from 1974 until present are supported. The function will default to
#' the current year. The function will return a data.frame with all countries and
#' all country codes. If more than one year is given, a single data.frame will be
#' returned with all codes and names for the given years, unless `simplify` is set
#' to `FALSE`, then a list of the same length of the input will be returned instead.
#' If `raw_response` is set to `TRUE`, a parsed data.frame will be returned along
#' with each raw response from the API. It is not possible to return a single
#' data.frame when `raw_response` is set to `TRUE`.
#'
#' If notes are enables with `include_notes`, a column `note` will be added to the
#' data.frame. Notes are stated with a code reference that has the following order
#' (alpha-2, alpha-3, num-3, SSB-3), for example Norway (NO, NOR, 578, 000). See
#' [Statistics Norway's webpage](https://www.ssb.no/klass/klassifikasjoner/552) for
#' more information on the code specification.
#'
#' @inheritParams get_municipalities
#'
#' @return data.frame or list
#'
#' @export
get_countries <- function(
    year = format(Sys.Date(), '%Y'), include_notes = FALSE, simplify = TRUE,
    raw_response = FALSE)
{
  x <- lapply(year, get_klass_codes_single, type = 'country',
              include_notes = include_notes, raw_response = raw_response)
  if (!raw_response & simplify) {
    x <- do.call('rbind', x)
  }
  x
}

#' get_klass_codes_single
#' @noRd
get_klass_codes_single <- function(
    year, type = c('municipality', 'county', 'country', 'both'),
    include_notes = FALSE, raw_response = FALSE)
{
  if (type == 'both') {
    r_m <- get_klass_codes_single(year, 'municipality')
    r_c <- get_klass_codes_single(year, 'county')
    r_m$county <- substring(r_m$code, 1, 2)
    colnames(r_c) <- c('year', 'county', 'county_name')
    r_c <- r_c[, c('county', 'county_name')]
    ret <- merge(r_m, r_c, by = 'county')
    return(ret[,c('year', 'code', 'name', 'county', 'county_name')])
  }
  x <- switch(
    type,
    'municipality' = klass_municipalities,
    'county' = klass_counties,
    'country' = klass_countries)
  endpoint <- get_klass_api_endpoint(
    year, type = type, links = x$endpoint, from = x$valid_from, to = x$valid_to
  )
  req <- request(endpoint) |>
    req_headers(Accept = if (raw_response) 'application/json' else 'text/csv')
  resp <- send_query(req)
  parsed <- parse_klass(resp, type, year, include_notes)
  if (raw_response) {
    parsed <- make_api_object(resp, parsed)
  }
  parsed
}
