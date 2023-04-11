#' null2na
#' @noRd
null2na <- function(x, na_value = NA_character_ ) {
  if (is.null(x) || length(x) == 0) na_value else x
}

#' request_brreg
#' @noRd
request_brreg <- function(brreg_type = c('enheter', 'roller'), entity = NULL, type = NULL) {
  brreg_type <- match.arg(brreg_type)
  req <- request(brreg_url)
  if (brreg_type %in% c('enheter', 'roller')) {
    entity <- clean_entity_input(entity)
    is_number <- grepl('\\d+', entity) && !grepl('\\D', entity)
    if (is_number) {
      verify_entity_number(entity)
      if (brreg_type == 'enheter') {
        if (type == 'both') {
          req <- req |>
            req_url_path_append('enheter') |>
            req_url_path_append(entity)
          # Query for main entity
          resp <- send_query(req, throttle_rate = 2)
          # If 404 -> retry as subentity
          if (resp$status_code == 404) {
            req$url <- sub('enheter', 'underenheter', req$url)
            resp <- send_query(req, throttle_rate = 2)
          }
          return(resp)
        } else if (type == 'main') {
          req <- req |>
            req_url_path_append('enheter') |>
            req_url_path_append(entity)
        } else if (type == 'sub') {
          req <- req |>
            req_url_path_append('underenheter') |>
            req_url_path_append(entity)
        }
      } else if (brreg_type == 'roller')  {
        req <- req |>
          req_url_path_append('enheter') |>
          req_url_path_append(entity) |>
          req_url_path_append('roller')
      }
    } else {
      req <- req |>
        req_url_path_append('enheter') |>
        req_url_query('navn' = entity)
    }
  }
  send_query(req)
}

#' parse_brreg_entity
#' @param parsed Output of `parse_response()`
#' @noRd
parse_brreg_entity <- function(parsed) {
  if ('organisasjonsnummer' %in% names(parsed)) {
    df <- parse_brreg_entity_single(parsed)
  } else if ('_embedded' %in% names(parsed)) {
    dl <- lapply(parsed$`_embedded`$enheter, parse_brreg_entity_single)
    df <- do.call('rbind', dl)
  } else if (length(names(parsed) == 2)) { # "_links" "page"
    cli::cli_warn('No entities found.')
    return(invisible(NULL))
  }
  df
}

#' parse_brreg_entity_single
#' @noRd
parse_brreg_entity_single <- function(p) {

  al <- brreg_address(p)

  # Create data.frame
  data.frame(
    organisasjonsnummer = p$organisasjonsnummer,
    navn = p$navn,
    organisasjonsform_kode = null2na(p$organisasjonsform$kode),
    organisasjonsform_beskrivelse = null2na(p$organisasjonsform$beskrivelse),
    forretningsadresse = al$forretningsadresse,
    beliggenhetsadresse = al$beliggenhetsadresse,
    kommune = al$kommune,
    land = al$land,
    postadresse = null2na(paste_brreg_address(p$postadresse)),
    internettadresse = null2na(p$hjemmeside),
    antall_ansatte = null2na(p$antallAnsatte),
    registeringsdato = null2na(p$registreringsdatoEnhetsregisteret),
    naeringskode_kode = null2na(p$naeringskode1$kode),
    naeringskode_beskrivelse = null2na(p$naeringskode1$beskrivelse),
    sektorkode_kode = null2na(p$institusjonellSektorkode$kode),
    sektorkode_beskrivelse = null2na(p$institusjonellSektorkode$beskrivelse),
    registrert_mvaregisteret = null2na(p$registrertIMvaregisteret),
    registrert_foretaksregisteret = null2na(p$registrertIForetaksregisteret),
    registrert_stiftelsesregisteret = null2na(p$registrertIStiftelsesregisteret),
    registrert_frivillinghetsregisteret = null2na(p$registrertIFrivillighetsregisteret),
    overordnet_enhet = null2na(p$overordnetEnhet),
    konkurs = null2na(p$konkurs),
    under_avvikling = null2na(p$underAvvikling)
  )
}

#' parse_brreg_roles
#' @param parsed Output of `parse_response()`
#' @inheritParams get_roles
#' @noRd
parse_brreg_roles <- function(parsed, entity) {
  p <- lapply(parsed$rollegrupper, function(x) x$roller[[1]])
  dl <- purrr::map2(p, entity, parse_brreg_roles_single)
  do.call('rbind', dl)
}

#' parse_brreg_roles_single
#' @noRd
parse_brreg_roles_single <- function(p, entity) {
  if (any(grepl('person', names(p)))) {
    data.frame(
      aktornummer = entity,
      type_kode = p$type$kode,
      type_beskrivelse = null2na(p$type$beskrivelse),
      navn = paste(p$person$navn$fornavn,
                   p$person$navn$etternavn),
      fodselsdato = null2na(p$person$fodselsdato),
      er_doed = null2na(p$person$erDoed, NA),
      organisasjonsnummer = NA_character_,
      organisasjonsform_kode = NA_character_,
      organisasjonsform_beskrivelse = NA_character_,
      er_slettet = NA,
      fratraadt = null2na(p$fratraadt, NA),
      rekkefolge = null2na(p$rekkefolge, NA_integer_)
    )
  } else {
    data.frame(
      aktornummer = entity,
      type_kode = p$type$kode,
      type_beskrivelse = null2na(p$type$beskrivelse),
      navn = paste(unlist(p$enhet$navn), collapse = ' '),
      fodselsdato = NA_character_,
      er_doed = NA,
      organisasjonsnummer = p$enhet$organisasjonsnummer,
      organisasjonsform_kode = null2na(p$enhet$organisasjonsform$kode),
      organisasjonsform_beskrivelse = null2na(p$enhet$organisasjonsform$beskrivelse),
      er_slettet = null2na(p$enhet$erSlettet, NA),
      fratraadt = null2na(p$fratraadt),
      rekkefolge = null2na(p$rekkefolge)
    )
  }
}

#' clean_entity_input
#' @noRd
clean_entity_input <- function(x) {
  # Get all digits
  d <- regmatches(x, gregexpr('\\d+', x)) |>
    unlist() |>
    paste(collapse = '')
  # Remove non-digit characters if
  # 'x' contains a nine-digit string
  if (grepl('\\d{9}', d)) {
    x <- gsub('\\D', '', x)
    return(x)
  }
  # Remove leading/trailing whitespace
  x <- trimws(x)
  # Remove extra whitespace
  x <- gsub('\\s+', ' ', x)
  x
}

#' verify_entity_number
#' @noRd
verify_entity_number <- function(x) {
  msg <- '{x} is not a valid organization number'
  if (grepl('\\d{9}', x)) {
    if (!is_valid_entity(x)) return(cli::cli_abort(msg))
  } else {
    return(cli::cli_abort(msg))
  }
  invisible(TRUE)
}

#' is_valid_entity
#' Check validity of Norwegian organization numbers
#' @noRd
is_valid_entity <- function(x) {
  weights <- c(3,2,7,6,5,4,3,2)
  # Early return if number doesn't have 9 digits
  if (nchar(x) != 9) return(FALSE)
  # Early return if number doesn't start with 8 or 9
  if (!grepl('^[8-9]', x)) return(FALSE)
  # Calculate mod11 check
  x <- as.integer(unlist(strsplit(x, '')))
  k <- sum(weights * x[1:8]) %% 11
  if (k == 0) k == x[9] else (11 - k) == x[9]
}

#' paste_brreg_address
#' @noRd
paste_brreg_address <- function(x) {
  address <- unlist(x$adresse)
  n <- length(address)
  if (!'postnummer' %in% names(x)) x$postnummer <- ''
  if (!'poststed' %in% names(x)) x$poststed <- ''
  if (is.null(address)) {
    out <- sprintf('%s %s', x$postnummer, x$poststed)
  } else if (n == 1) {
    out <- sprintf('%s, %s %s', address, x$postnummer, x$poststed)
  } else {
    address <- paste(address, collapse = ', ')
    out <- sprintf('%s, %s %s', address, x$postnummer, x$poststed)
  }
  out <- gsub('\\s+', ' ', out)
  out
}

#' brreg_address
#' @noRd
brreg_address <- function(p){
  # forretningsadresse
  if (!is.null(p$forretningsadresse)) {
    forretningsadresse <- paste_brreg_address(p$forretningsadresse)
    beliggenhetsadresse <- NA_character_
  } else if (!is.null(p$beliggenhetsadresse)) {
    forretningsadresse <- NA_character_
    beliggenhetsadresse <- paste_brreg_address(p$beliggenhetsadresse)
  } else {
    forretningsadresse <- NA_character_
    beliggenhetsadresse <- NA_character_
  }
  # kommune
  if (!is.null(p$forretningsadresse$kommune)) {
    kommune <- p$forretningsadresse$kommune
  } else if (!is.null(p$beliggenhetsadresse$kommune)) {
    kommune <- p$beliggenhetsadresse$kommune
  } else {
    kommune <- NA_character_
  }
  # land
  if (!is.null(p$forretningsadresse$land)) {
    land <- p$forretningsadresse$land
  } else if (!is.null(p$beliggenhetsadresse$land)) {
    land <- p$beliggenhetsadresse$land
  } else {
    land <- NA_character_
  }
  list(
    forretningsadresse = forretningsadresse,
    beliggenhetsadresse = beliggenhetsadresse,
    kommune = kommune,
    land = land
  )
}
