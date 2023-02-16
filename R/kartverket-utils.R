#' request_kartverket
#' @noRd
request_kartverket <- function(
    type = c('sok', 'punktsok'),
    search = NULL,
    lat = NULL,
    lon = NULL,
    radius = NULL,
    crs = NULL,
    ...) {

  type <- match.arg(type)

  # Create query parameters
  if (type == 'sok')
    params <- create_params_kv_sok(search = search, crs = crs, ...)
  else
    params <- create_params_kv_punktsok(
      lat = lat, lon = lon,
      crs = crs, radius = radius, ...)

  # Create query
  req <- request(kv_url) %>%
    req_url_path_append('v1') |>
    req_url_path_append(type) |>
    req_url_query(!!!params)

  send_query(req)

}

#' Query parameters for Get Address
#'
#' Accepted query parameters for Kartverket's endpoint `/sok`.
#'
#' @inheritParams get_address_info
#' @param fuzzy If TRUE a fuzzy search on the provided addresses is conducted.
#'   Not compatible with the use of wildcards (*).
#' @param search_mode Modify the `search` parameter with boolean logic. Use
#'   this to select whether the search will require each search parameter to be
#'   found in the match, or whether it is sufficient with hits on only one
#'   parameter. E.g. `search` = 'munkegata 1 trondheim' & `search_mode`
#'   = 'OR' will return everything that contains 'munkegata' and / or number '1'
#'   and / or 'trondheim'. Defaults to 'AND'.
#' @param object_type Specify the type of address to query, either road
#'   address ('Vegadresse'), cadastral address ('Matrikkeladresse') or both.
#'   Defaults to both.
#' @param mun_code A character string with a Norwegian municipality code.
#' @param mun_name A character string with the name of a Norwegian municipality.
#' @param address_name A character string with the name of a street, road, path,
#'   space or area that is entered in the cadastre.
#' @param address_text A character string with the name of a street, road, path,
#'   space or area that is entered in the cadastre.
#' @param address_ext_name A character string with a inherited property unit
#'   name, or the name of an institution or building that is used as part of an
#'   official address.
#' @param address_code An integer value that uniquely identifies addressable
#'   streets, roads, paths, places and areas that are entered in the cadastre
#'   within the municipality.
#' @param address_number An integer value representing part of an address number
#'   (house number), e.g. 23.
#' @param address_letter A character string representing part of an address
#'   number (house number), e.g. the 'B' in 23B. To search only for addresses
#'   without a letter, you include 'letter =' in the search string without
#'   filling in any value.
#' @param cadastral_unit_number An integer value with the cadastral unit number
#'   for the property. This is part of a cadastral address where road addresses
#'   has not been introduced, or the road address's link to cadastral unit.
#' @param property_unit_number An integer value with the property unit number
#'   for the property. This is part of a cadastral address where road addresses
#'   has not been introduced, or the road address's link to cadastral unit.
#' @param leasehold_number An integer value with the leasehold number for the
#'   property. This is part of a cadastral address where road addresses has not
#'   been introduced, or the road address's link to cadastral unit.
#' @param sub_code An integer value with subcode (undernummer) for cadastral
#'   addresses with the same cadastral, property and leasehold number.
#' @param unit_number An integer value with the unit number (formerly called flat
#'   number) for an apartment in multi-dwelling buildings. The letter and the
#'   first two numbers indicate the floor, the last two indicate the apartment's
#'   number on the floor, counted from left to right. For example: 'H0102',
#'   'K0101'.
#' @param postal_town A character string with the name of a Norwegian postal
#'   town (place).
#' @param postal_code A character string with a Norwegian postal code.
#' @param ascii_compatible If TRUE (default) the returned data is ASCII
#'  compatible.
#' @param page An integer value with the page to query. Defaults to 0 (the first
#'   page).
#' @param size An integer value with the size for each page. Defaults to 1000.
#'
#' @keywords internal
#'
create_params_kv_sok <- function(
    search = NULL,
    fuzzy = FALSE,
    search_mode = c('AND', 'OR'),
    object_type = c('Both', 'Vegadresse', 'Matrikkeladresse'),
    mun_code = NULL,
    mun_name = NULL,
    address_name = NULL,
    address_text = NULL,
    address_ext_name = NULL,
    address_code = NULL,
    address_number = NULL,
    address_letter = NULL,
    cadastral_unit_number = NULL,
    property_unit_number = NULL,
    leasehold_number = NULL,
    sub_code = NULL,
    unit_number = NULL,
    postal_town = NULL,
    postal_code = NULL,
    crs = 4258,
    ascii_compatible = TRUE,
    page = 0,
    size = 1000
) {

  search_mode <- match.arg(search_mode)
  object_type <- match.arg(object_type)
  if (object_type == 'Both') object_type <- NULL

  params <- list(
    sok = search,
    fuzzy = fuzzy,
    sokemodus = search_mode,
    objtype = object_type,
    kommunenummer = mun_code,
    kommunenavn = mun_name,
    adressenavn = address_name,
    adressetekst = address_text,
    adressetilleggsnavn = address_ext_name,
    adressekode = address_code,
    nummer = address_number,
    bokstav = address_letter,
    gardsnummer = cadastral_unit_number,
    bruksnummer = property_unit_number,
    festenummer = leasehold_number,
    undernummer = sub_code,
    bruksenhetsnummer = unit_number,
    poststed = postal_town,
    postnummer = postal_code,
    utkoordsys = crs,
    treffPerSide = size,
    side = page,
    asciiKompatibel = ascii_compatible
  )

  return(params)
}

#' Query parameters for Find Address
#'
#' Accepted query parameters for Kartverket's endpoint `/punktsok`.
#'
#' @inheritParams find_address_from_point
#' @inheritParams create_params_kv_sok
#' @keywords internal
create_params_kv_punktsok <- function(
    lat, lon,
    radius,
    crs,
    ascii_compatible = TRUE,
    size = 1000,
    page = 0
) {

  # Split CRS
  crs <- handle_crs(crs)

  params <- list(
    lat = lat,
    lon = lon,
    radius = radius,
    koordsys = crs$crs_in,
    utkoordsys = crs$crs_out,
    treffPerSide = size,
    side = page,
    asciiKompatibel = ascii_compatible
  )

  return(params)

}

#' parse_kartverket
#' @noRd
parse_kartverket <- function(resp, parsed, x, radius, crs, ...) {
  parsed_adresser <- parsed$adresser
  n_total <- parsed$metadata$totaltAntallTreff
  n <- nrow(parsed_adresser)
  if (n_total > n) {
    msg <- 'Found more addresses than was returned on the first page by the API.'
    if (rlang::is_interactive()) { #interactive()
      cli::cli_alert_warning(msg)
      user_yes <- kv_ask()
      if (user_yes) {
        iter <- ceiling((n_total - n) / n)
        tmp_resp <- vector('list', iter+1)
        tmp_parsed <- vector('list', iter+1)
        tmp_resp[[1]] <- resp
        tmp_parsed[[1]] <- parsed
        for (i in seq_len(iter)) {
          tmp_resp[[i+1]] <-
            request_kartverket(
              'punktsok', lat = x[1], lon = x[2], radius = radius,
              crs = crs, page = i, ...)
          tmp_parsed[[i+1]] <-
            parse_response(tmp_resp[[i+1]], simplifyVector = TRUE)
        }
        resp <- tmp_resp
        parsed_adresser <- purrr::map_df(tmp_parsed, function(x) x$adresser)
      }
    } else {
      bullet <- 'Found {n_total} addresses in total, while {n} are currently retrieved.'
      cli::cli_warn(c(msg, i = bullet))
    }
  }
  list(resp = resp, parsed = parsed_adresser)
}


#' handle_crs
#' @noRd
handle_crs <- function(crs) {
  if (suppressWarnings(any(is.na(as.integer(crs))))) {
    cli::cli_abort(c(
      'crs must be coercible to an integer',
      x = 'You entered {crs}'))
  }
  if (length(crs) == 1) {
    crs_in <- crs_out <- crs
  } else if (length(crs) == 2){
    crs_in <- crs[1]
    crs_out <- crs[2]
  } else {
    cli::cli_abort('`crs` has incorrect length')
  }
  list(crs_in = crs_in, crs_out = crs_out)
}

#' kv_ask
#' @noRd
kv_ask <- function() {
  if (testthat::is_testing()) return(TRUE)
  switch(
    utils::menu(c('Yes', 'No'), title = 'Do you wish to retrieve the remaining results?'),
    TRUE, FALSE
  )
}

#' kv_flatten_res
#' @noRd
kv_flatten_res <- function(res, x) {

  # Convert 'bruksenhetsnummer' to comma seperated string
  res$parsed$bruksenhetsnummer <-
    lapply(res$parsed$bruksenhetsnummer, \(x) paste0(x, collapse = ', ')) |>
    unlist()

  # Flatten nested list w/ 'representasjonspunkt'
  res$parsed$epsg <- res$parsed$representasjonspunkt$epsg
  res$parsed$lon <- res$parsed$representasjonspunkt$lon
  res$parsed$lat <- res$parsed$representasjonspunkt$lat
  res$parsed$representasjonspunkt <- NULL

  res
}

