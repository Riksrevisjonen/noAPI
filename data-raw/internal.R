user_agent <- "noAPI (riksrevisjonen.github.io/noAPI)"
brreg_url <- 'https://data.brreg.no/enhetsregisteret/api/'
nbank_url <- 'https://data.norges-bank.no/api/'
kv_url <- 'https://ws.geonorge.no/adresser/'

usethis::use_data(
  user_agent,
  brreg_url,
  nbank_url,
  kv_url,
  internal = TRUE,
  overwrite = TRUE)

