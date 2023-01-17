#' Get exchange rate
#'
#' Fetch exchange rates from \emph{Norges Bank}.
#'
#' @inheritParams get_entity
#' @param currency The currency to fetch data for.
#' @param frequency Frequency of data. Can be daily, monthly or annual. Defaults to
#'   daily.
#' @param n_obs The number of observations to fetch. See details.
#' @param start The first date to fetch data for.
#' @param end The last date to fetch data for.
#'
#' @details
#' Currency must be formatted as a three letter
#' [ISO 3166](https://www.iso.org/iso-3166-country-codes.html) code. A list
#' of available country codes can be found at
#' [Norges Bank](https://www.norges-bank.no/en/topics/Statistics/exchange_rates/).
#' You can fetch observations for several currencies by passing a character vector.
#' If you want to include all currencies, you can set the `currency` argument
#' to `ALL`. Note that the number of observations is limited to 20 when
#' retrieving all currencies.
#'
#' By default, the 10 latest observations are returned. You can adjust the number
#' of observations by setting `n_obs` to a positive integer. You can also fetch
#' exchange rates for a specific time period by setting the arguments `start`
#' and `end`. Both arguments accept a date in the format `%Y-%m-%d` (_YYYY-MM-DD_).
#'
#' The function returns a data.frame by default. If you set `raw_response` to
#' `TRUE`, the raw response from the API will be returned instead. Note that the
#' response will be returned silently when `raw_response` is set to `TRUE`.
#'
#' @return data.frame or list
#'
#' @export
#' @examples
#' # Get latest daily exchange rates
#' get_exchange_rate('EUR')
#' # Get monthly exchange rates for multiple currencies
#' get_exchange_rate(c('EUR', 'USD'), frequency = 'monthly')
#' # Get last 5 observations for all currencies
#' get_exchange_rate('ALL', n_obs = 5)
#'
get_exchange_rate <- function(currency, frequency = c('daily', 'monthly', 'annual'),
                              n_obs = 10, start = NULL, end = NULL,
                              raw_response = FALSE) {
  frequency <- match.arg(frequency)
  dynamic <- if (any(is.null(start), is.null(end))) TRUE else FALSE
  dl <- lapply(toupper(currency), get_exchange_rate_single,
    frequency = frequency, start = start,
    end = end, dynamic = dynamic, n_obs = n_obs, raw_response = raw_response
  )
  if (raw_response) return(invisible(dl))
  dl <- do.call(rbind, dl)
  dl
}

#' get_exchange_rate_single
#' @noRd
get_exchange_rate_single <- function(currency, frequency, dynamic, n_obs, start,
                                     end, raw_response) {
  if (!grepl('^[A-Z4]{3}$', currency)) {
    msg <- c(
      '{.var currency} is not valid.',
      'i' = '{currency} does not conform to the required regex pattern.',
      'x' = 'The country code must be exactly 3 letters with regex pattern [A-Z4].'
    )
    cli::cli_abort(msg)
  }
  frequency <- switch(
    frequency,
    'daily' = 'B',
    'monthly' = 'M',
    'annual' = 'A',
    'B'
  )
  if (currency == 'ALL') {
    currency <- ''
    if (n_obs > 20) {
      msg <- 'When retrieving all currencies, the maximum number of observations is set to 20.'
      cli::cli_warn(msg)
      n_obs <- 20
    }
  }
  if (dynamic) {
    resp <- request_sdmx_latest(currency, frequency, n_obs)
  } else {
    resp <- request_sdmx(currency, frequency, start, end)
  }
  parsed <- parse_response(resp, simplifyVector = FALSE)
  if (raw_response) {
    out <- make_api_object(resp, parsed)
  } else {
    out <- parse_sdmx(parsed)
  }
  return(out)
}
