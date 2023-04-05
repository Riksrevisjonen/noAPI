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

#' check_digits
#' @noRd
check_digits <- function(x) {
  x <- as.integer(x)
  x <- floor(log10(x))+1
  if (all(x %in% c(3,4))) {
    out <- 'kommune'
  } else if (all(x %in% c(1,2))) {
    out <- 'fylke'
  } else {
    msg <- 'You cannot retrive data for counties and municipalities at the same time.'
    cli::cli_abort(
      c(msg,
        'x' = 'You provided both county (1-2 digits) and municipal (3-4 digits) in the same function call.',
        'i' = 'Please clean your data or make seperate queries.'
      )
    )
  }
  out
}

#' get_schools_api_endpoint
#' @noRd
get_schools_api_endpoint <- function(x) {
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

#' parse_nsr
#' @noRd
parse_nsr <- function(type = c('kommune', 'fylke', 'alle'), resp) {
  type <- match.arg(type)
  parsed <- resp_body_json(resp, simplifyVector = TRUE)
  if (type == 'alle') parsed <- parsed$Enheter
  parsed
}
