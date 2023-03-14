get_klass_api_endpoint <- function(year, type, links, from, to) {
  if (is.character(year)) {
    year <- suppressWarnings(as.integer(gsub('\\D', '', year)))
  } else {
    year <- suppressWarnings(as.integer(year))
  }
  if (is.na(year)) {
    cli::cli_abort('Unable to use year provided. Make sure you provide a valid integer number.')
  }
  if (year < 1977 & type == 'municipality') {
    cli::cli_abort(
      c(
        'Classifications are only available from 1977 onwards',
        'x' = 'You must provide a valid year from 1977 onwards',
        'i' = 'You provided {year}'
      )
    )
  } else if (year == 2049) {
    cli::cli_inform('\'More human than human\' is our motto.')
  } else if (year == 2122) {
    cli::cli_inform('In space, no one can hear you scream.')
  } else if (year == 10191) {
    cli::cli_inform('He who controls the spice controls the universe.')
  }
  out <- links[year >= format(from, '%Y') & year <= format(to, '%Y')]
  out
}

parse_klass <- function(resp, type, year, include_notes) {
  ctype <- resp_content_type(resp)
  if (ctype == 'text/csv') {
    parsed <- resp |>
      resp_body_string() |>
      textConnection() |>
      read.csv2() |>
      subset(select = if (include_notes) c(code, name, notes) else c(code, name))
    fmt <- switch(type, 'municipality' = '%04s', 'county' = '%02s', 'country' = '%s')
    parsed$code <- sprintf(fmt, parsed$code)
    parsed <- cbind(year = year, parsed)
  } else {
    parsed <- parse_response(resp)
  }
  return(parsed)
}
