#' null2na
#' @noRd
null2na <- function(x) if (is.null(x) || length(x) == 0) NA else x

#' request_brreg
#' @noRd
request_brreg <- function(type = c('enheter', 'kommuner'), entity = NULL) {
  type <- match.arg(type)
  req <- request(brreg_url)
  if (type == 'enheter') {
    is_valid_number <- grepl('\\d{9}', entity)
    #
    if (nchar(entity) != 9 && !grepl('\\D', entity) ) { #grepl('\\d', entity) && grepl('\\D', entity)
      msg = '{entity} is not a 9-digit organization number'
      cli::cli_abort(msg)
    }
    if (is_valid_number) {
      req <- req |>
        req_url_path_append('enheter') |>
        req_url_path_append(entity)
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
    organisasjonsform.kode = null2na(p$organisasjonsform$kode),
    organisasjonsform.beskrivelse = null2na(p$organisasjonsform$beskrivelse),
    forretningsadresse = null2na(sprintf(
      '%s, %s %s', unlist(p$forretningsadresse$adresse),
      p$forretningsadresse$postnummer, p$forretningsadresse$poststed)),
    kommune = null2na(p$forretningsadresse$kommune),
    postadresse = null2na(sprintf(
      '%s, %s %s', unlist(p$postadresse$adresse),
      p$postadresse$postnummer, p$postadresse$poststed)),
    internettadresse = null2na(p$hjemmeside),
    antall_ansatte = null2na(p$antallAnsatte),
    registeringsdato = null2na(p$registreringsdatoEnhetsregisteret),
    naeringskode.kode = null2na(p$naeringskode1$kode),
    naeringskode.beskrivelse = null2na(p$naeringskode1$beskrivelse),
    sektorkode.kode = null2na(p$institusjonellSektorkode$kode),
    sektorkode.beskrivelse = null2na(p$institusjonellSektorkode$beskrivelse),
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
