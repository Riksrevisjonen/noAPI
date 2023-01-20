#' Get address info
#'
#' Search for Norwegian addresses in
#' \emph{The Norwegian Mapping Authority's Cadastre and Land Registry}.
#'
#' @details
#' Many users will get by with using the regular `search` parameter, but more
#' advanced users might want to look into the wide range of possible parameters.
#' For a list of all accepted query parameters see [create_params_kv_sok()]
#' or refer to the
#' [API documentation](https://ws.geonorge.no/adresser/v1/#/default/get_sok)
#' for further details (in Norwegian only).

#' The function returns a data.frame or list by default. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned instead. Note that the
#' response will then be returned silently.
#'
#' @param search A Norwegian address. For example: 'munkegata 1 trondheim'.
#' @param crs The desired coordinate reference system for the output. Default
#'   value is ETRS 89 for latitude and longitude data (EPSG:4258).
#' @param ... Additional arguments passed to the API call. See details.
#' @inheritParams get_entity
#'
#' @return data.frame or list
#' @export
#' @examples
#'
#' # A simple query
#' df <- get_address_info('munkegata 1 trondheim')
#'
#' # Use the advanced parameters to be more specific
#' df <- get_address_info(address_name = 'Munkegata', address_number = 1,
#'          mun_name = 'Trondheim')
#'
#' \dontrun{
#' # Multiple adresses
#' res <- get_address_info(c('munkegata 1 trondheim', 'tromsø gate'))
#'
#' # A more advanced query
#' res <- get_address_info(c('munkegata 1 trondheim', 'munkegata 1'),
#'    mun_code = c(NULL, '5001'))
#'
#' }
get_address_info <- function(search = NULL, crs = 4258, ..., raw_response = FALSE) {
  if (!is.null(search)) {
    dl <- mapply(get_address_info_safe, search = search,
                 crs = crs, ..., raw_response = raw_response,
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    dl <- mapply(get_address_info_safe, crs = crs,
                 ..., raw_response = raw_response,
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }
  if (raw_response) return(invisible(dl))
  if (length(dl) == 1) dl <- dl[[1]]
  dl <- dl[lengths(dl) != 0]
  dl
}

#' Get address info (single)
#' @inheritParams get_address_info
#' @noRd
get_address_info_single <- function(search = NULL, crs = 4258,
                                    ..., raw_response = FALSE) {
  resp <- request_kartverket('sok', search, crs = crs, ...)
  parsed <- parse_response(resp, simplifyVector = TRUE)
  if (length(parsed$adresser) == 0) {
    cli::cli_warn('No addresses found. Returning NULL.')
    return(NULL)
  }
  # Parse results again (and check for remaining results)
  res <- parse_kartverket(resp, parsed, crs = crs, ...)
  if (raw_response) {
    out <- make_api_object(res$resp, res$parsed)
  } else {
    out <- res$parsed
  }
  out
}

#' Get address info (safe method)
#' @inheritParams get_address_info
#' @noRd
get_address_info_safe <-
  purrr::possibly(get_address_info_single, otherwise = NULL, quiet = FALSE)

#' Find addresses within a geographical area
#'
#' Search for Norwegian addresses within a certain radius from a geographical
#' point.
#'
#' @details Advanced users might want to look into the additional possible
#' parameters. For a list of all accepted query parameters see
#' [create_params_kv_punktsok()] or refer to the [API
#' documentation](https://ws.geonorge.no/adresser/v1/#/default/get_sok) for
#' further details (in Norwegian only).
#'
#' The function returns a data.frame or list by default. If you set
#' `raw_response` to `TRUE`, the raw response from the API will be returned
#' instead. Note that the response will then be returned silently.
#'
#' @param x A vector or list with latitude and longitude coordinates.
#' @param radius An integer value with the radius for the query, in whole
#'   meters.
#' @param crs An integer or vector with the input and output coordinate
#'   reference system. Default value is ETRS 89 for latitude and longitude data
#'   (EPSG:4258).
#' @inheritParams get_address_info
#' @inheritParams get_entity
#'
#' @return data.frame or list
#' @export
#' @examples
#' # Using a vector
#' df <- find_address_from_point(c(lat = 63.42805, lon = 10.39679))
#'
#' # Using a list
#' dl <- find_address_from_point(
#'   list(c(lat = 59.91364, lon = 10.7508),
#'        c(lat = 63.42805, lon = 10.39679)))
#'
#' # Using a data.frame
#' dl <- find_address_from_point(
#'     data.frame(lat = c(59.91364, 63.42805),
#'                lon = c(10.7508, 10.39679)))
#'
find_address_from_point <- function(x, radius = 50, crs = 4258, ...,
                                    raw_response = FALSE) {
  UseMethod('find_address_from_point')
}

#' find_address_from_point (numeric)
#' @inheritParams find_address_from_point
#' @export
find_address_from_point.numeric <- function(x, radius = 50, crs = 4258, ...,
                                            raw_response = FALSE) {
  res <- find_address_from_point_safe(x, radius = radius, crs = crs,
                                      raw_response = raw_response)
  if (raw_response) return(invisible(res))
  res
}

#' find_address_from_point (list)
#' @inheritParams find_address_from_point
#' @export
find_address_from_point.list <- function(x, radius = 50, crs = 4258, ...,
                                         raw_response = FALSE) {
  dl <- lapply(x, find_address_from_point.numeric, radius = radius, crs = crs,
               ..., raw_response = raw_response)
  if (raw_response) return(invisible(dl))
  if (length(dl) == 1) dl <- dl[[1]]
  dl <- dl[lengths(dl) != 0]
  dl
}

#' find_address_from_point (data.frame)
#' @inheritParams find_address_from_point
#' @export
find_address_from_point.data.frame <- function(x, radius = 50, crs = 4258, ...){

  if (!all(c('lat', 'lon') %in% colnames(x)))
    cli::cli_abort('data.frame must have columns with names lat and lon')

  lapply(1:nrow(x), function(i) {
    coords <- c(x[i, 'lat'], x[i, 'lon'])
    find_address_from_point_safe(coords, radius = radius, crs = crs)
  })
}

#' find_address_from_point (single method)
#' @inheritParams find_address_from_point
#' @noRd
find_address_from_point_single <- function(x, radius, crs, ...,
                                           raw_response = FALSE) {
  if (length(x) != 2)
    cli::cli_abort('`x` must be of length 2 (latitude, longitude)')

  if (all(c('lat', 'lon') %in% names(x))) {
    x <- c(x[['lat']], lon = x[['lon']])
  }

  # Call API
  resp <- request_kartverket(
    'punktsok', lat = x[1], lon = x[2],
    radius = radius, crs = crs, ...)

  # Parse response
  parsed <- parse_response(resp, simplifyVector = TRUE)
  if (length(parsed$adresser) == 0) {
    cli::cli_warn('No addresses found. Returning NULL.')
    return(NULL)
  }

  # Parse results again (and check for remaining results)
  res <- parse_kartverket(resp, parsed, x, radius, crs, ...)

  if (raw_response) {
    out <- make_api_object(res$resp, res$parsed)
  } else {
    out <- res$parsed
  }
  out
}

#' find_address_from_point (safe method)
#' @inheritParams find_address_from_point
#' @noRd
find_address_from_point_safe <-
  purrr::possibly(find_address_from_point_single, otherwise = NULL,
                  quiet = FALSE)

