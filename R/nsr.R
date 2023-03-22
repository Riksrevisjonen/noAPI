#' Get schools
#'
#' Fetch information from the National School Registry (NSR).
#'
#' @details
#' The function returns a data.frame by default. If you prefer the output
#' as a list you can set `simplify` to `FALSE`. This can be useful to keep
#' programmatically track of failed queries. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned together with the
#' parsed response. Note that the response will then be returned silently.
#'
#' See the
#' [API documentation](https://www.udir.no/om-udir/data/nxr/l)
#' for further details (in Norwegian only).
#'
#' @param county A Norwegian county code
#' @param municipality A Norwegian municipality code
#' @inheritParams get_entity
#' @export
#' @examples
#' # Get schools by county
#' get_schools(county = 11)
#'
#' # Get schools by municipality
#' get_schools(municipality = 1101)
#'
#' \dontrun{
#' # Get all schools
#' df <- get_schools()
#' }
get_schools <- function(county = NULL, municipality = NULL,
                        simplify = TRUE, raw_response = FALSE) {
  common_info(simplify, raw_response)
  l <- get_schools_api_endpoint(county, municipality)
  if (l$type == 'alle') {
    dl <- get_schools_safe(type = 'alle', raw_response = raw_response)
    if (raw_response) return(invisible(dl))
    if (!simplify) return(list(dl))
  } else {
    dl <- lapply(l$unit, get_schools_safe, type = l$type, raw_response = raw_response)
    if (raw_response) return(invisible(dl))
    if (simplify) return(do.call('rbind', dl))
  }
  dl
}

#' get_schools_single
#' @noRd
get_schools_single <- function(unit = NULL,
                               type = c('kommune', 'fylke', 'alle'),
                               raw_response = FALSE) {
  type <- match.arg(type)
  resp <- request_nsr(type, unit)
  parsed <- parse_nsr(type, resp)
  if (raw_response) {
    out <- make_api_object(resp, parsed)
  } else {
    out <- parsed
  }
  out
}

#' get_schools_safe
#' @inheritParams get_schools_single
#' @noRd
get_schools_safe <-
  purrr::possibly(get_schools_single, otherwise = NULL, quiet = FALSE)
