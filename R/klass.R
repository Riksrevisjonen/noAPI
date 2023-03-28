#' Get municipalities and counties
#'
#' Get all Norwegian municipalities or counties, and their respective codes,
#' for a given year.
#'
#' `get_municipalities()` supports all years from 1977, while `get_counties()`
#' supports all years from 1972. `get_adm_units()` is a wrapper function to get
#' both municipality and county codes in the same function call. All three
#' functions default to the current year.
#'
#' The functions returns a data.frame by default. If you prefer the output
#' as a list you can set `simplify` to `FALSE`. This can be useful to keep
#' programmatically track of failed queries. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned together with the
#' parsed response. Note that the response will then be returned silently.
#'
#' @inheritParams get_entity
#' @param year The year for which the codes should be valid for.
#' @param include_notes If notes should be included or not.
#'
#' @return data.frame or list
#'
#' @export
#' @examples
#'
#' # Get municipality codes
#' get_municipalities()
#'
#' # Get county codes
#' get_municipalities()
#'
#' # Get both
#' get_adm_units()
get_municipalities <- function(
    year = format(Sys.Date(), '%Y'), include_notes = FALSE,
    simplify = TRUE, raw_response = FALSE) {

  common_info(simplify, raw_response)

  dl <- lapply(year, get_klass_codes_safe, type = 'municipality',
               include_notes = include_notes, raw_response = raw_response)
  if (raw_response) return(dl)
  if (simplify) return(do.call('rbind', dl))
  dl
}

#' @rdname get_municipalities
#' @export
get_counties <- function(
    year = format(Sys.Date(), '%Y'), include_notes = FALSE, simplify = TRUE,
    raw_response = FALSE) {
  common_info(simplify, raw_response)
  dl <- lapply(year, get_klass_codes_safe, type = 'county',
               include_notes = include_notes, raw_response = raw_response)
  if (raw_response) return(dl)
  if (simplify) return(do.call('rbind', dl))
  dl
}

#' @rdname get_municipalities
#' @export
get_adm_units <- function(year = format(Sys.Date(), '%Y'), simplify = TRUE) {
  dl <- lapply(year, get_klass_codes_safe, type = 'both',
               include_notes = include_notes, raw_response = FALSE)
  if (simplify) return(do.call('rbind', dl))
  dl
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
#' All years from 1974 until present are supported. The function defaults to
#' the current year.
#'
#' If notes are enables with `include_notes`, a column `note` will be added to the
#' data.frame. Notes are stated with a code reference that has the following order
#' (alpha-2, alpha-3, num-3, SSB-3), for example Norway (NO, NOR, 578, 000). See
#' [Statistics Norway's webpage](https://www.ssb.no/klass/klassifikasjoner/552) for
#' more information on the code specification.
#'
#' The function returns a data.frame by default. If you prefer the output
#' as a list you can set `simplify` to `FALSE`. This can be useful to keep
#' programmatically track of failed queries. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned together with the
#' parsed response. Note that the response will then be returned silently.
#'
#' @inheritParams get_municipalities
#'
#' @return data.frame or list
#'
#' @export
#' @examples
#' # Get country for current year
#' get_countries()
get_countries <- function(
    year = format(Sys.Date(), '%Y'), include_notes = FALSE, simplify = TRUE,
    raw_response = FALSE) {
  common_info(simplify, raw_response)
  dl <- lapply(year, get_klass_codes_safe, type = 'country',
               include_notes = include_notes, raw_response = raw_response)
  if (raw_response) return(dl)
  if (simplify) return(do.call('rbind', dl))
  dl
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

#' Get klass (safe method)
#' @inheritParams get_klass_codes_single
#' @noRd
get_klass_codes_safe <-
  purrr::possibly(get_klass_codes_single, otherwise = NULL, quiet = FALSE)

