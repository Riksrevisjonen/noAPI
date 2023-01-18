.onLoad <- function(libname, pkgname) {
  if (!Sys.getenv("NOAPI_DISABLE_CACHING") == "TRUE") {
    cm <- cachem::cache_mem(max_size = 512 * 1024^2, evict = "lru")
    # BRREG functions
    get_entity <<- memoise::memoise(get_entity, cache = cm)
    get_municipalities <<- memoise::memoise(get_municipalities, cache = cm)
    get_roles <<- memoise::memoise(get_roles, cache = cm)
    # Kartverket functions
    get_address_info <<- memoise::memoise(get_address_info, cache = cm)
    find_address_from_point <<- memoise::memoise(find_address_from_point, cache = cm)
  }
}

.onAttach <- function(libname, pkgname) {
  if (!Sys.getenv("NOAPI_DISABLE_CACHING") == "TRUE") {
    packageStartupMessage("Info: Session based caching is enabled.")
  }
}
