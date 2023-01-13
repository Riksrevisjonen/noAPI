#' Get entity
#'
#' Collect basic information about an entity from the \emph{Central Coordinating
#' Register for Legal Entities} (Enhetsregisteret) based on the entity's
#' organisation number.
#'
#' See the API documentation for further details
#' \url{https://data.brreg.no/enhetsregisteret/api/docs/index.html} (in
#' Norwegian only).
#'
#' @param entity A Norwegian organisation number or company name.
#' @param raw_response  If TRUE a list of class `noAPI` is
#'   returned, including the raw `httr2_response`.
#' @return data.frame or list
#' @export
#' @examples
#' # Get entity by number
#' df <- get_entity(974760843)
#' # Get entity by name
#' df <- get_entity('Riksrevisjonen')
#' # Get multiple entites
#' df <- get_entity(c(974760843, 971524960))
#'
get_entity <- function(entity, raw_response = FALSE) {
  dl <- lapply(entity, function(x) get_entity_safe(x, raw_response))
  if (raw_response) return(dl)
  do.call('rbind', dl)
}

#' Get municipalities
#'
#' Collect the names and keys for Norwegian municipalities from the
#' \emph{Central Coordinating Register for Legal Entities} (Enhetsregisteret).
#'
#' @inheritParams get_entity
#' @return data.frame or list
#' @export
#' @examples
#' # Get municipalities
#' df <- get_municipalities()
#'
get_municipalities <- function(raw_response = FALSE) {

  resp <- request_brreg('kommuner')
  parsed <- resp_body_json(resp, simplifyVector = TRUE)

  if (raw_response) {
    out <- make_api_object(resp, parsed)
  }
  else {
    out <- parsed$`_embedded`$kommuner[,1:2]
  }
  return(out)

}

#' Get entity (single method)
#' @inheritParams get_entity
#' @noRd
get_entity_single <- function(entity, raw_response = FALSE) {
  resp <- request_brreg('enheter', entity)
  parsed <- parse_response(resp, simplifyVector = FALSE)
  if (raw_response) {
    out <- make_api_object(resp, parsed)
  }
  else {
    out <- parse_brreg_entity(parsed)
  }
  return(out)
}

# Get entity (safe)
#' @inheritParams get_entity
#' @noRd
get_entity_safe <-
  purrr::possibly(get_entity_single, quiet = FALSE)
