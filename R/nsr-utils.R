#' request_nsr
#' @noRd
request_nsr <- function(type = c('kommune', 'fylke', 'alle'), unit = NULL){
  type <- match.arg(type)
  req <- request(nsr_url)
  if (type == 'kommune') {
    if (nchar(unit) != 4 && !grepl('\\d{4}', unit, fixed = TRUE)) {
      cli::cli_abort(
        c('Invalid municipality code',
          'x' = 'A municipality code must be four digits',
          'i' = 'You provided {unit}')
      )
    }
    req <- req |>
      req_url_path_append('kommune') |>
      req_url_path_append(unit)
  } else if (type == 'fylke') {
    if (nchar(unit) != 2 && !grepl('\\d{2}', unit, fixed = TRUE)) {
      cli::cli_abort(
        c('Invalid county code',
          'x' = 'A county code must be two digits',
          'i' = 'You provided {unit}')
      )
    }
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

#' get_schools_api_endpoint
#' @noRd
get_schools_api_endpoint <- function(county, municipality) {
  if (is.null(county) && is.null(municipality)) {
    type <- 'alle'
    unit <- NULL
  } else if (!is.null(county) && is.null(municipality)) {
    type <- 'fylke'
    unit <- county
  } else if (is.null(county) && !is.null(municipality)) {
    type <- 'kommune'
    unit <- municipality
  } else {
    msg <- 'You cannot retrive data for counties and municipalities at the same time'
    cli::cli_abort(
      c(msg,
        'x' = 'You cannot set both county and municipality equal to not NULL',
        'i' = 'You provided county: {county} and municipality: {municipality}'
      )
    )
  }
  list(type = type, unit = unit)
}


#' parse_nsr
#' @noRd
parse_nsr <- function(type = c('kommune', 'fylke', 'alle'), resp) {
  type <- match.arg(type)
  parsed <- resp_body_json(resp, simplifyVector = TRUE)
  if (type == 'alle') parsed <- parsed$Enheter
  parsed
}
