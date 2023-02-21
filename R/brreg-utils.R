#' null2na
#' @noRd
null2na <- function(x) if (is.null(x) || length(x) == 0) NA else x

#' request_brreg
#' @noRd
request_brreg <- function(type = c('enheter', 'roller','kommuner'), entity = NULL) {
  type <- match.arg(type)
  req <- request(brreg_url)
  if (type %in% c('enheter', 'roller')) {
    entity <- clean_entity_input(entity)
    is_number <- grepl('\\d+', entity) && !grepl('\\D', entity)
    if (is_number) {
      verify_entity_number(entity)
      req <- req |>
        req_url_path_append('enheter') |>
        req_url_path_append(entity)
      if (type == 'roller')  {
        req <- req |>
          req_url_path_append('roller')
      }
    } else {
      req <- req |>
        req_url_path_append('enheter') |>
        req_url_query('navn' = entity)
    }
  } else if (type == 'kommuner') {
    req <- req |>
      req_url_path_append('kommuner') |>
      req_url_query(size = 450)
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
    # NOTE: Old code from riksrevR. Not sure this is needed.
    # if (length(parsed$`_embedded`$enheter) == 0) {
    #   cli::cli_warn('No entities found.')
    #   return(invisible(NULL))
    # }
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
  # Create data.frame
  data.frame(
    organisasjonsnummer = p$organisasjonsnummer,
    navn = p$navn,
    organisasjonsform_kode = null2na(p$organisasjonsform$kode),
    organisasjonsform_beskrivelse = null2na(p$organisasjonsform$beskrivelse),
    forretningsadresse = null2na(sprintf(
      '%s, %s %s', unlist(p$forretningsadresse$adresse),
      p$forretningsadresse$postnummer, p$forretningsadresse$poststed)),
    kommune = null2na(p$forretningsadresse$kommune),
    postadresse = null2na(sprintf(
      '%s, %s %s', unlist(paste0(p$postadresse$adresse, collapse = '; ')),
      p$postadresse$postnummer, p$postadresse$poststed)),
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
    under_avvikling = null2na(p$underAvvikling),
    stringsAsFactors = FALSE
  )
}

#' parse_brreg_roles
#' @param parsed Output of `parse_response()`
#' @inheritParams get_roles
#' @noRd
parse_brreg_roles <- function(parsed, entity) {
  p <- lapply(parsed$rollegrupper, function(x) x$roller[[1]])
  dl <- purrr::map2(p, entity, parse_brreg_roles_single)
  dl_entities <- lapply(dl, function(x) {
    if ('organisasjonsnummer' %in% names(x)) x else NULL
  })
  dl_persons <- lapply(dl, function(x) {
    if (!'organisasjonsnummer' %in% names(x)) x else NULL
  })
  list(persons = do.call('rbind', dl_persons),
       entities = do.call('rbind', dl_entities))
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
      er_doed = null2na(p$person$erDoed),
      fratraadt = null2na(p$fratraadt),
      rekkefolge = null2na(p$rekkefolge)
    )
  } else {
    data.frame(
      aktornummer = entity,
      type_kode = p$type$kode,
      type_beskrivelse = null2na(p$type$beskrivelse),
      organisasjonsnummer = p$enhet$organisasjonsnummer,
      navn = paste(unlist(p$enhet$navn), collapse = ' '),
      organisasjonsform_kode = null2na(p$enhet$organisasjonsform$kode),
      organisasjonsform_beskrivelse = null2na(p$enhet$organisasjonsform$beskrivelse),
      er_slettet = null2na(p$enhet$erSlettet),
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
  x <- gsub('  +', ' ', x)
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
