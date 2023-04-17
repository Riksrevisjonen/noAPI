#' Get entity
#'
#' Fetch information about an entity from the
#' _Central Coordinating Register for Legal Entities_ (Enhetsregisteret)
#' based on the entity's name or organisation number.
#'
#' @details
#' Users should be aware that queries for entity names and numbers are handled
#' somewhat differently. By default (`type = NULL`) queries for _numbers_
#' will first send requests to the registry for _main entities_, but will then
#' also query the registry for _sub-entities_ in case no information is found
#' in the first query. This ensures that any entity number still in use
#' will return a valid response. For _names_ however `get_entity()` will only
#' query the registry for _main entities_, unless `type` is explicitly set to
#' `sub`. Further information on the use of sub-entities can be found
#' [here](https://www.brreg.no/bedrift/underenhet/).
#'
#' The function returns a data.frame by default. If you prefer the output
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
#' @param type Optional. Type entity to query. Either main, sub or NULL
#'  (default). See details.
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
#' # Get information on sub-entities (number)
#' df <- get_entity(999178197)
#'
#' #' # Get information on sub-entities (name)
#' df <- get_entity('Kuben Yrkesarena', type = 'sub')
#'
get_entity <- function(entity, type = NULL,
                       simplify = TRUE, raw_response = FALSE) {
  common_info(simplify, raw_response)
  if (!is.null(type)) type <- match.arg(type, c('main', 'sub'))
  dl <- lapply(entity, function(x)
    get_brreg_safe(x, type = type, brreg_type = 'enheter', raw_response))
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
#' The function returns a data.frame by default. If you prefer the output
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
#' df <- get_roles(974760843)
#'
#' # Get roles for a mulitiple enities
#' df <- get_roles(c(974760843, 971524960))
#'
get_roles <- function(entity, simplify = TRUE, raw_response = FALSE) {
  common_info(simplify, raw_response)
  dl <- lapply(entity, function(x)
    get_brreg_safe(x, type = NULL, brreg_type = 'roller', raw_response))
  if (raw_response) return(invisible(dl))
  if (simplify) return(do.call('rbind', dl))
  dl
}

#' Get brreg (single method)
#' @inheritParams get_entity
#' @noRd
get_brreg_single <- function(entity, type = NULL,
                             brreg_type = c('enheter', 'roller'),
                             raw_response = FALSE) {
  brreg_type <- match.arg(brreg_type)
  resp <- request_brreg(brreg_type, entity, type)
  parsed <- parse_response(resp, simplifyVector = FALSE)
  if (raw_response) {
    out <- make_api_object(resp, parsed)
  }
  else {
    if (brreg_type == 'enheter') {
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
