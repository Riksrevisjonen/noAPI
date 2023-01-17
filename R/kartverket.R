#' Get address info
#'
#' Search for Norwegian addresses in
#' \emph{The Norwegian Mapping Authority's Cadastre and Land Registry}.
#'
#' @details
#' Many users will get by with using the regular `search` parameter, but more
#' advanced users might want to look into the wide range of possible parameters.
#' For a list of all accepted query parameters see [create_params_kartverket()]
#' or refer to the
#' [API documentation](https://ws.geonorge.no/adresser/v1/#/default/get_sok)
#' for further details (in Norwegian only).

#' The function returns a data.frame or list by default. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned instead. Note that the
#' response will then be returned silently.
#'
#' @param search A Norwegian address. For example: 'munkegata 1 trondheim'.
#' @param ... Additional arguments to send to the API call. See details.
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
get_address_info <- function(search = NULL, ..., raw_response = FALSE) {
  if (!is.null(search)) {
    dl <- mapply(get_address_info_safe, search = search, ...,
                 SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    dl <- mapply(get_address_info_safe, ...,
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
get_address_info_single <- function(search = NULL, ..., raw_response = FALSE) {
  resp <- request_kartverket('sok', search, ...)
  parsed <- parse_response(resp, simplifyVector = TRUE)
  if (length(parsed$adresser) == 0) {
    cli::cli_warn('No addresses found.')
    # cli::cli_warn(c(msg, i = 'Check your spelling.'))
    return(NULL)
  }
  n_total <- parsed$metadata$totaltAntallTreff
  if (n_total > nrow(parsed$adresser)) {
    msg <- 'There are more addresses than was returned on the first page by the API.'
    cli::cli_warn(c(msg, i = 'Try increasing the `size` parameter.'))
  }
  if (raw_response) {
    out <- make_api_object(resp, parsed)
  } else {
    out <- parsed$adresser
  }
  out
}

#' Get address info (safe method)
#' @inheritParams get_address_info
#' @noRd
get_address_info_safe <-
  purrr::possibly(get_address_info_single, otherwise = NULL, quiet = FALSE)
