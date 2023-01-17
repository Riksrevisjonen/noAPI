#' request_kartverket
#' @noRd
request_kartverket <- function(type = c('sok', 'punktsok'), search, ...) {

  type <- match.arg(type)

  # Create query parameters
  params <- create_params_kartverket(search = search, ...)

  # Create query
  req <- request(kv_url) %>%
    req_url_path_append('v1') |>
    req_url_path_append(type) |>
    req_url_query(!!!params)

  send_query(req)

}

#' Query parameters for Kartverket
#'
#' Accepted query parameters for Kartverket.
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
#' @param size An integer value with the size for each page. Defaults to 100.
#' @param crs The desired coordinate reference system.
#'
#' @keywords internal
#'
create_params_kartverket <- function(
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
    ascii_compatible = TRUE,
    page = 0,
    size = 1000,
    crs = 4258
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
