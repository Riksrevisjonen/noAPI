#' Get entity
#'
#' Fetch information about an entity from the
#' _Central Coordinating Register for Legal Entities_ (Enhetsregisteret)
#' based on the entity's name or organisation number.
#'
#' @details
#'
#' The function returns a data.frame by default. If you prefer the the output
#' as a list you can set `simplify` to `FALSE`. This can be useful to keep
#' programmatically track of failed queries. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned together with the
#' parsed response. Note that the response will then be returned silently.
#'
#' See the
#' [API documentation](https://data.brreg.no/enhetsregisteret/api/docs/index.html)
#' for further details (in Norwegian only).
#'
#' @param entity A Norwegian organisation number or company name.
#' @param simplify If `TRUE` (default), a single data.frame is returned. Ignored
#'   if `raw_response` is set to `TRUE`.
#' @param raw_response  If `TRUE` a list of class `noAPI` is
#'   returned, including the raw `httr2_response`.
#' @return data.frame or list
#' @export
#' @examples
#' # Get entity by number
#' df <- get_entity(974760843)
#'
#' # Get entity by name
#' df <- get_entity('Riksrevisjonen')
#'
#' # Get multiple entities
#' df <- get_entity(c(974760843, 971524960))
#'
get_entity <- function(entity, simplify = TRUE, raw_response = FALSE) {
  common_info(simplify, raw_response)
  dl <- lapply(entity, function(x)
    get_brreg_safe(x, type = 'enheter', raw_response))
  if (raw_response) return(invisible(dl))
  if (simplify) return(do.call('rbind', dl))
  dl
}

#' Get roles
#'
#' Fetch information about entity roles from the
#' _Central Coordinating Register for Legal Entities_ (Enhetsregisteret) based
#' on the organisation number.
#'
#' @details
#' The function returns a data.frame by default. If you prefer the the output
#' as a list you can set `simplify` to `FALSE`. This can be useful to keep
#' programmatically track of failed queries. If you set `raw_response`
#' to `TRUE`, the raw response from the API will be returned together with the
#' parsed response. Note that the response will then be returned silently.
#'
#' See the
#' [API documentation](https://data.brreg.no/enhetsregisteret/api/docs/index.html)
#' for further details (in Norwegian only).
#'
#' @param entity A Norwegian organisation number.
#' @inheritParams get_entity
#' @return list
#' @export
#' @seealso `get_entity()`. It's not possible to get roles for an entity using a
#'   name search. If you do not have an organisation number, you should first do
#'   a call to `get_entity()` and then call `get_roles()` using the response from
#'   the first call.
#' @examples
#' # Get roles for a single entity
#' res <- get_roles(974760843)
#'
#' # Get roles for a mulitiple enities
#' res <- get_roles(c(974760843, 971524960))
#'
get_roles <- function(entity, simplify = TRUE, raw_response = FALSE) {
  common_info(simplify, raw_response)
  dl <- lapply(entity, function(x)
    get_brreg_safe(x, type = 'roller', raw_response))
  if (raw_response) return(invisible(dl))
  if (simplify) return(do.call('rbind', dl))
  dl
}

#' Get brreg (single method)
#' @inheritParams get_entity
#' @noRd
get_brreg_single <- function(entity,
                             type = c('enheter', 'roller'),
                             raw_response = FALSE) {
  type <- match.arg(type)
  resp <- request_brreg(type, entity)
  parsed <- parse_response(resp, simplifyVector = FALSE)
  if (raw_response) {
    out <- make_api_object(resp, parsed)
  }
  else {
    if (type == 'enheter') {
      out <- parse_brreg_entity(parsed)
    } else {
      out <- parse_brreg_roles(parsed, entity)
    }
  }
  out
}

#' Get brreg (safe method)
#' @inheritParams get_brreg_single
#' @noRd
get_brreg_safe <-
  purrr::possibly(get_brreg_single, otherwise = NULL, quiet = FALSE)
