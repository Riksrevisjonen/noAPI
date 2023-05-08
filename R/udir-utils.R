#' request_udir
#' @noRd
request_udir <- function(type = c('kommune', 'fylke', 'alle'), unit = NULL,
                         api_type = c('skole', 'barnehage')){
  type <- match.arg(type)
  api_type <- match.arg(api_type)
  if (api_type == 'skole') u <- nsr_url else u <- nbr_url
  req <- request(u)
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

#' get_udir_api_endpoint
#' @noRd
get_udir_api_endpoint <- function(x) {
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
    type <- check_digits_udir(x)
    unit <- x
  }
  list(type = type, unit = unit)
}

#' parse_udir
#' @noRd
parse_udir <- function(type = c('kommune', 'fylke', 'alle'), resp) {
  type <- match.arg(type)
  parsed <- resp_body_json(resp, simplifyVector = TRUE)
  if (type == 'alle') parsed <- parsed$Enheter
  parsed
}


#' check_digits_udir
#' @noRd
check_digits_udir <- function(x) {
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
