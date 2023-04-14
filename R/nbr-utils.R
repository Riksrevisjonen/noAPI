#' request_nbr
#' @noRd
request_nbr <- function(type = c('kommune', 'fylke', 'alle'), unit = NULL){
  type <- match.arg(type)
  req <- request(nbr_url)
  if (type == 'kommune') {
    if (is.numeric(unit)) unit <- sprintf('%04d', unit)
    if (is.character(unit)) unit <- sprintf('%04s', unit)
    req <- req |>
      req_url_path_append('kommune') |>
      req_url_path_append(unit)
  } else if (type == 'fylke') {
    if (is.numeric(unit)) unit <- sprintf('%02d', unit)
    if (is.character(unit)) unit <- sprintf('%02s', unit)
    req <- req |>
      req_url_path_append('fylke') |>
      req_url_path_append(unit)
  } else if (type == 'alle') {
    params <- list(sidenummer = 1, antallPerSide = 20000)
    req <- req |>
      req_url_query(!!!params)
  }
  send_query(req)
}

#' get_kindergartens_api_endpoint
#' @noRd
get_kindergartens_api_endpoint <- function(x) {
  if (length(x) > 1 & any(grepl('all', x))) {
    msg <- "You cannot use 'all' and unit codes at the same time."
    cli::cli_abort(
      c(msg,
        'i' = 'Please clean your data or make seperate queries.'
      )
    )
  }
  if (x[1] == 'all') {
    type <- 'alle'
    unit <- NULL
  } else {
    type <- check_digits(x)
    unit <- x
  }
  list(type = type, unit = unit)
}

#' parse_nbr
#' @noRd
parse_nbr <- function(type = c('kommune', 'fylke', 'alle'), resp) {
  type <- match.arg(type)
  parsed <- resp_body_json(resp, simplifyVector = TRUE)
  if (type == 'alle') parsed <- parsed$Enheter
  parsed
}
