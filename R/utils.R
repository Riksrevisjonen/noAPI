#' @import httr2
NULL

#' check_api
#' @param u URL
#' @noRd
check_api <- function(u) {
  request(u) |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform() |>
    resp_is_error()
}

#' send_query
#' @param req A httr2 request
#' @param max_tries Maximum number of retries
#' @param throttle_rate Max number of requests per second
#' @noRd
send_query <- function(req, max_tries = 3, throttle_rate = 1) {
  resp <- req %>%
    req_user_agent(user_agent) %>%
    req_headers("Accept-Encoding" = "gzip") %>%
    req_retry(
      is_transient = ~ resp_status(.x) %in% c(429, 503),
      max_tries = max_tries
    ) %>%
    req_throttle(rate = throttle_rate) %>% #
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()
  resp
}

#' parse_response
#' @param resp A httr2 response
#' @noRd
parse_response <- function(resp, simplifyVector = TRUE){
  # Parse response based on content type
  type <- resp_content_type(resp)
  resp_error <- resp_is_error(resp)
  if (resp_error) {
    cli::cli_abort('The API returned an error: {resp$status_code} {resp_status_desc(resp)}')
  }
  if (type == "application/json") {
    parsed <- resp_body_json(resp, simplifyVector = simplifyVector)
  }
  return(parsed)
}

#' make_api_object
#' @noRd
make_api_object <- function(resp, parsed) {
  structure(
    list(
      url = resp$url,
      status = resp$status_code,
      content = parsed,
      response = resp
    ),
    class = "noAPI"
  )
}

#' noAPI print method
#' @noRd
print.noapi <- function(x, ...) {
  cat("<URL ", x$url, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}
