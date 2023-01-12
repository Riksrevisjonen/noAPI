user_agent <- "noAPI (riksrevisjonen.github.io/noAPI)"
brreg_url <- 'https://data.brreg.no/enhetsregisteret/api/'

usethis::use_data(
  user_agent,
  brreg_url,
  internal = TRUE,
  overwrite = TRUE)

