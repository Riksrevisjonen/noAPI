#' request_ssb
#' @noRd
request_ssb <- function(url) {

  resp <- url |>
    request() |>
    send_query()

  ssb_data <- read.csv2(textConnection(httr2::resp_body_string(resp), encoding = 'UTF-8'), encoding = 'UTF-8')

  ssb_data <- clean_column_names(ssb_data)

}

#' clean_table
#' @noRd
clean_column_names <- function(x) {

  column_names <- colnames(x)

  column_names <- column_names|>
    tolower() |>
    gsub('([^[:alnum:]])', ' ', x = _) |>
    trimws() |>
    gsub('\\s+', '_', x = _)

  colnames(x) <- column_names

  return(x)
}
