#' Get municipalities
#'
#' Get all Norwegian municipalities and municipality codes for a given year.
#'
#' Fetches all Norwegian municipalities and municipality codes for a given year.
#' All years from 1977 until present are supported. The function will default to
#' the current year. The function will return a data.frame with all municipalities.
#' If more than one year is given, a single data.frame will be returned with all
#' codes and names for the given years, unless `simplify` is set to `FALSE`, then
#' a list will be returned instead.
#'
#' You can add county numbers and names for each municipality by setting `add_county`
#' to `TRUE`.
#'
#' @inheritParams get_entity
#' @param year The year for which the codes should be valid for
#' @param add_county If set to `TRUE`, add county name and number
#'
#' @return data.frame or list
#'
#' @seealso `get_counties()` for county codes and names
#'
#' @export
get_municipalities <- function(
    year = format(Sys.Date(), '%Y'), add_county = FALSE, simplify = TRUE,
    raw_response = FALSE)
{
  if (raw_response) simplify <- FALSE
  if (add_county) {
    raw_response <- FALSE
    x <- lapply(year, get_region_codes_single, type = 'both', raw_response = raw_response)
  } else {
    x <- lapply(year, get_region_codes_single, type = 'municipality', raw_response = raw_response)
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
#' If more than one year is given, a single data.frame will be returned with all
#' codes and names for the given years, unless `simplify` is set to `FALSE`, then
#' a list will be returned instead.
#'
#' @inheritParams get_municipalities
#'
#' @return data.frame or list
#'
#' @seealso `get_municipalities()` for municipality codes and names
#'
#' @export
get_counties <- function(
    year = format(Sys.Date(), '%Y'), simplify = TRUE, raw_response = FALSE)
{
  if (raw_response) simplify <- FALSE
  x <- lapply(year, get_region_codes_single, type = 'county', raw_response = raw_response)
  if (!raw_response & simplify) {
    x <- do.call('rbind', x)
  }
  x
}

#' get_region_codes_single
#' @noRd
get_region_codes_single <- function(
    year, type = c('municipality', 'county', 'both'), raw_response = FALSE)
{
  if (type == 'both') {
    r_m <- get_region_codes_single(year, 'municipality')
    r_c <- get_region_codes_single(year, 'county')
    r_m$county <- substring(r_m$code, 1, 2)
    colnames(r_c) <- c('year', 'county', 'county_name')
    r_c <- r_c[, c('county', 'county_name')]
    ret <- merge(r_m, r_c, by = 'county')
    return(ret[,c('year', 'code', 'name', 'county', 'county_name')])
  }
  x <- switch(type, 'municipality' = klass_municipalities, 'county' = klass_counties)
  endpoint <- get_municipality_api_endpoint(
    year, type = type, links = x$endpoint, from = x$valid_from, to = x$valid_to
  )
  req <- request(endpoint) %>%
    req_headers(Accept = if (raw_response) 'application/json' else 'text/csv')
  resp <- send_query(req)
  parsed <- parse_klass(resp, type, year)
  if (raw_response) {
    parsed <- make_api_object(resp, parsed)
  }
  parsed
}
