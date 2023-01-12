.onLoad <- function(libname, pkgname) {
  if (!Sys.getenv("NOAPI_DISABLE_CACHING") == "TRUE") {
    cm <- cachem::cache_mem(max_size = 512 * 1024^2, evict = "lru")
    # BRREG functions
    get_entity <<- memoise::memoise(get_entity, cache = cm)
    get_municipalities <<- memoise::memoise(get_municipalities, cache = cm)
    # ...
  }
}

.onAttach <- function(libname, pkgname) {
  if (!Sys.getenv("PIPR_DISABLE_CACHING") == "TRUE") {
    packageStartupMessage("Info: Session based caching is enabled.")
  }
}
