#' Get address info
#'
#' Search for Norwegian addresses in
#' \emph{The Norwegian Mapping Authority's Cadastre and Land Registry}.
#'
#' @details
#' For most use cases ETRS89 (EPSG:4258) and WSG84 (EPSG:4326) can be considered
#' equal, but users should be aware hat there is a slight shift on both axes
#' between the two coordinate systems. If high precision is needed `crs` should
#' be set to 4326.
#'
#' Many users will get by with using the regular `search` parameter, but more
#' advanced users might want to look into the wide range of possible parameters.
#' For a list of all accepted query parameters see [create_params_kv_sok()]
#' or refer to the
#' [API documentation](https://ws.geonorge.no/adresser/v1/#/default/get_sok)
#' for further details (in Norwegian only).
#'
#' The function returns a data.frame by default. If you prefer the output
#' as a list you can set `simplify` to `FALSE`. This can be useful to keep
#' programmatically track of failed queries. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned together with the
#' parsed response. Note that the response will then be returned silently.
#'
#' @param search A Norwegian address. For example: 'munkegata 1 trondheim'.
#' @param crs The desired coordinate reference system for the output. Default
#'   value is ETRS89 for latitude and longitude data (EPSG:4258). See details.
#' @param ... Additional arguments passed to the API call.
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
#' df <- get_address_info(c('munkegata 1 trondheim', 'tromsø gate'))
#'
#' # A more advanced query
#' df <- get_address_info(c('munkegata 1 trondheim', 'storgata 1'),
#'    mun_code = list(NULL, '0301'))
#'
#' }
get_address_info <- function(search = NULL, crs = 4258, ...,
                             simplify = TRUE,
                             raw_response = FALSE) {
  common_info(simplify, raw_response)
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
  if (simplify) return(do.call('rbind', dl))
  dl
}

#' Get address info (single)
#' @inheritParams get_address_info
#' @noRd
get_address_info_single <- function(search = NULL, crs = 4258,
                                    ..., raw_response = FALSE) {
  # Get input params
  p <- list(search = search, ...)
  qp <- paste(sprintf('%s=%s', names(p), p), collapse = ',')

  # Send query
  resp <- request_kartverket('sok', search, crs = crs, ...)

  # Parse response
  parsed <- parse_response(resp, simplifyVector = TRUE)
  if (length(parsed$adresser) == 0) {
    cli::cli_warn("Did not find any addresses for the following query: '{cli::col_br_yellow(qp)}'.")
    return(NULL)
  }

  # Parse results again (and check for remaining results)
  res <- parse_kartverket(resp, parsed, search = search, crs = crs, ...)
  res <- kv_flatten_res(res)

  # Add input to result
  if (all(names(p) == 'search')) {
    res$parsed$sok <- search
  } else {
    res$parsed$sok <- qp
  }
  res$parsed <- res$parsed[c(ncol(res$parsed), (1:ncol(res$parsed)-1))]

  # Return
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
#' @details
#' For most use cases ETRS89 (EPSG:4258) and WSG84 (EPSG:4326) can be considered
#' equal, but users should be aware that there is a slight shift on both axes
#' between the two coordinate systems. If high precision is needed `crs` should
#' be set to 4326.
#'
#' Advanced users might want to look into the additional possible
#' parameters. For a list of all accepted query parameters see
#' [create_params_kv_punktsok()] or refer to the [API
#' documentation](https://ws.geonorge.no/adresser/v1/#/default/get_sok) for
#' further details (in Norwegian only).
#'
#' The function returns a data.frame by default. If you prefer the output
#' as a list you can set `simplify` to `FALSE`. This can be useful to keep
#' programmatically track of failed queries. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned together with the
#' parsed response. Note that the response will then be returned silently.
#'
#' @param x A vector or list with latitude and longitude coordinates.
#' @param radius An integer value with the radius for the query, in whole
#'   meters.
#' @param crs An integer or vector with the input and output coordinate
#'   reference system. Default value is ETRS89 for latitude and longitude data
#'   (EPSG:4258).
#' @param closest If TRUE only the address that is closest to the geographical
#'   point is returned.
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
find_address_from_point <- function(x, radius = 50, crs = 4258,
                                    closest = FALSE, ...,
                                    simplify = TRUE,
                                    raw_response = FALSE) {
  UseMethod('find_address_from_point')
}

#' @method find_address_from_point numeric
#' @export
find_address_from_point.numeric <- function(x, radius = 50, crs = 4258,
                                            closest = FALSE, ...,
                                            simplify = TRUE,
                                            raw_response = FALSE) {
  common_info(simplify, raw_response)
  res <- find_address_from_point_safe(x, radius = radius, crs = crs,
                                      closest = closest,
                                      raw_response = raw_response)
  if (raw_response) return(invisible(res))
  if (!simplify) return(list(res))
  res
}

#' @method find_address_from_point list
#' @export
find_address_from_point.list <- function(x, radius = 50, crs = 4258,
                                         closest = FALSE, ...,
                                         simplify = TRUE,
                                         raw_response = FALSE) {
  dl <- lapply(x, find_address_from_point.numeric, radius = radius, crs = crs,
               closest = closest, ..., raw_response = raw_response)
  if (raw_response) return(invisible(dl))
  if (simplify) return(do.call('rbind', dl))
  dl
}

#' find_address_from_point (single method)
#' @inheritParams find_address_from_point
#' @noRd
find_address_from_point_single <- function(x, radius, crs, closest, ...,
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
    x <- paste(x, collapse = ', ')
    cli::cli_warn("Did not find any addresses for the following point: '{cli::col_br_yellow(x)}'.")
    return(NULL)
  }

  # Parse results again (and check for remaining results)
  res <- parse_kartverket(resp, parsed, x = x, radius = radius, crs = crs, ...)
  res <- kv_flatten_res(res, x)

  # Add input to result
  res$parsed$punkt <- list(x)
  res$parsed <- res$parsed[c(ncol(res$parsed), (1:ncol(res$parsed)-1))]

  # Select the closest address
  if (closest) {
    res$parsed <- res$parsed[
      which.min(res$parsed$meterDistanseTilPunkt),]
  }

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

