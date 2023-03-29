#' get_klass_api_endpoint
#' @noRd
get_klass_api_endpoint <- function(year, type, links, from, to) {
  if (is.character(year)) {
    year <- suppressWarnings(as.integer(gsub('\\D', '', year)))
  } else {
    year <- suppressWarnings(as.integer(year))
  }
  if (is.na(year)) {
    cli::cli_abort('Unable to use year provided. Make sure you provide a valid integer number.')
  }
  min_year <- switch(
    type,
    'municipality' = 1977,
    'county' = 1841,
    'country' = 1974,
    'sic' = 1994
  )
  if (year < min_year || year > 9999) {
    msg <- if (year > 9999) 'up to 9999' else 'from {min_year} onwards'
    cli::cli_abort(c(
      paste0('Classifications are only available ', msg),
      x = 'You must provide a valid year',
      i = 'You provided {year}'
    ))
  } else if (year == 2049) {
    cli::cli_inform('\'More human than human\' is our motto.')
  } else if (year == 2122) {
    cli::cli_inform('In space, no one can hear you scream.')
  }
  out <- links[year >= format(from, '%Y') & year <= format(to, '%Y')]
  out
}

#' parse_klass
#' @importFrom utils read.csv2
#' @noRd
parse_klass <- function(resp, type, year, include_notes) {
  ctype <- resp_content_type(resp)
  if (ctype == 'text/csv') {
    parsed <- resp |>
      resp_body_string() |>
      textConnection() |>
      read.csv2()
    if (type == 'sic') {
      cols <- c('code', 'parentCode', 'level', 'shortName', 'name')
      if (include_notes) cols <- c(cols, 'notes')
    } else {
      cols <- if (include_notes) c('code', 'name', 'notes') else c('code', 'name')
    }
    parsed <- parsed[, cols]
    fmt <- switch(type, 'municipality' = '%04d', 'county' = '%02d', '%s')
    parsed$code <- sprintf(fmt, parsed$code)
    parsed <- cbind(year = year, parsed)
  } else {
    parsed <- parse_response(resp)
  }
  return(parsed)
}
