library(httr2)

user_agent <- "noAPI (riksrevisjonen.github.io/noAPI)"
brreg_url <- 'https://data.brreg.no/enhetsregisteret/api/'
nbank_url <- 'https://data.norges-bank.no/api/'
kv_url <- 'https://ws.geonorge.no/adresser/'
ssb_url <- 'https://data.ssb.no/api/'
nsr_url <- 'https://data-nsr.udir.no/v3/enheter/'

versions <- request(ssb_url) %>%
  req_url_path_append('klass/v1/classifications/131') %>%
  req_url_query(includeFuture = 'true') %>%
  req_perform() %>%
  resp_body_json()

parse_versions <- function(x) {
  data.frame(
    valid_from = Reduce(c, lapply(x, \(e) as.Date(e$validFrom))),
    valid_to = Reduce(c, lapply(x, \(e) {
      if (!is.null(e$validTo)) as.Date(e$validTo)-1 else as.Date('9999-12-31')
    })),
    endpoint = sapply(x, \(e) e$`_links`$self$href[1])
  )
}

klass_municipalities <- parse_versions(versions$versions)

vcounties <- request(ssb_url) %>%
  req_url_path_append('klass/v1/classifications/104') %>%
  req_url_query(includeFuture = 'true') %>%
  req_perform() %>%
  resp_body_json()

klass_counties <- parse_versions(vcounties$versions)

vcountries <- request(ssb_url) %>%
  req_url_path_append('klass/v1/classifications/552') %>%
  req_url_query(includeFuture = 'true') %>%
  req_perform() %>%
  resp_body_json()

klass_countries <- parse_versions(vcountries$versions)

vsic <- request(ssb_url) %>%
  req_url_path_append('klass/v1/classifications/6') %>%
  req_url_query(includeFuture = 'true') %>%
  req_perform() %>%
  resp_body_json()

klass_sic <- parse_versions(vsic$versions)

usethis::use_data(
  user_agent,
  brreg_url,
  nbank_url,
  kv_url,
  ssb_url,
  nsr_url,
  klass_municipalities,
  klass_counties,
  klass_countries,
  klass_sic,
  internal = TRUE,
  overwrite = TRUE)

