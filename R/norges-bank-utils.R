#' request_sdmx_latest
#' @noRd
request_sdmx_latest <- function(currency, frequency, n_obs) {
  api_path <- sprintf(
    'data/EXR/%s.%s.NOK.SP?format=sdmx-json&lastNObservations=%s&locale=en',
    frequency, currency, n_obs
  )
  req <- request(nbank_url) |> req_url_path_append(api_path)
  send_query(req)
}

#' request_sdmx
#' @noRd
request_sdmx <- function(currency, frequency, start, end) {
  api_path <- sprintf(
    'data/EXR/%s.%s.NOK.SP?format=sdmx-json&startPeriod=%s&endPeriod=%s&locale=en',
    frequency, currency, start, end
  )
  req <- request(nbank_url) |> req_url_path_append(api_path)
  send_query(req)
}

#' parse_sdmx_single
#' @noRd
parse_sdmx_single <- function(index, series, currency, frequency, str) {
  attr_idx <- series$attributes |> unlist() + 1
  attrs <- str$attributes$series
  dates_idx <- as.integer(names(series$observations)) + 1
  dates_values <- str$dimensions$observation[[1]]$values
  dates <- sapply(dates_idx, \(i) dates_values[[i]]$id, USE.NAMES = FALSE)
  dates_from <- sapply(dates_idx, \(i) dates_values[[i]]$start, USE.NAMES = FALSE)
  dates_to <- sapply(dates_idx, \(i) dates_values[[i]]$end, USE.NAMES = FALSE)
  values <- series$observations |> unlist() |> unname()
  # Create data.frame
  df <- data.frame(
    currency = rep(currency, length(values)),
    frequency = rep(frequency, length(values)),
    exch_rate = values,
    decimals = as.integer(rep(attrs[[1]]$values[[attr_idx[1]]]$id, length(values))),
    calculated = rep(attrs[[2]]$values[[attr_idx[2]]]$id, length(values)),
    unit_multiplier = rep(attrs[[3]]$values[[attr_idx[3]]]$name, length(values)),
    date_from = as.Date(dates_from, format = '%Y-%m-%dT%H:%M:%S'),
    date_to = as.Date(dates_to, format = '%Y-%m-%dT%H:%M:%S'),
    date = dates
  )
  df
}

#' parse_sdmx
#' @noRd
parse_sdmx <- function(resp) {
  data <- resp$data$dataSets[[1]]
  str <- resp$data$structure
  frequency <- str$dimensions$series[[1]]$values[[1]]$name
  currency <- sapply(str$dimensions$series[[2]]$values, \(x) x$id, USE.NAMES = FALSE)
  # For each dataset and currency, create a data.frame
  out <- mapply(
    parse_sdmx_single,
    seq_along(currency),
    data$series,
    currency,
    MoreArgs = list(frequency = frequency, str = str),
    SIMPLIFY = FALSE
  )
  # Return a single data.frame
  do.call(rbind, out)
}
