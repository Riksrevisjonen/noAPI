.onLoad <- function(libname, pkgname) {
  if (!Sys.getenv("NOAPI_DISABLE_CACHING") == "TRUE") {
    cm <- cachem::cache_mem(max_size = 512 * 1024^2, evict = "lru")
    # BRREG functions
    get_entity <<- memoise::memoise(get_entity, cache = cm)
    get_roles <<- memoise::memoise(get_roles, cache = cm)
    # Kartverket functions
    get_address_info <<- memoise::memoise(get_address_info, cache = cm)
    find_address_from_point <<- memoise::memoise(find_address_from_point, cache = cm)
    # SSB Klass functions
    get_municipalities <<- memoise::memoise(get_municipalities, cache = cm)
    get_counties <<- memoise::memoise(get_counties, cache = cm)
    get_countries <<- memoise::memoise(get_countries, cache = cm)
    get_adm_units <<- memoise::memoise(get_adm_units, cache = cm)
    # UDIR NSR functions
    get_schools <<- memoise::memoise(get_schools, cache = cm)
    get_kindergartens <<- memoise::memoise(get_kindergartens, cache = cm)
    # SSB ready-made dataset functions
    get_ssb_metadata <<- memoise::memoise(get_ssb_metadata, cache = cm)
    get_ssb_table <<- memoise::memoise(get_ssb_table, cache = cm)
  }
}

.onAttach <- function(libname, pkgname) {
  if (!Sys.getenv("NOAPI_DISABLE_CACHING") == "TRUE") {
    packageStartupMessage("Info: Session based caching is enabled.")
  }
}
