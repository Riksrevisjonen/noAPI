#' Get SSB table metadata
#'
#' Fetch metadata for ready-made tables from Statistics Norway (SSB).
#'
#' @param data_tag Optional. Data tag to filter on.
#'
#' @details
#' The function returns an overview of all the available ready-made
#' tables produced by Statistics Norway.
#'
#' Note that the metadata is updated daily at 05.00 and 11.30 AM, which can lead
#' to the API being unavailable for up to five minutes.
#'
#' See the [API documentation](https://data.ssb.no/api/) for further details.
#'
#' @seealso [get_ssb_table()] to retrieve the actual tables.
#' @return data.frame
#' @export
#' @examples
#' # Get all metadata
#' ssb_metadata <- get_ssb_metadata()
#'
#' # Get a subset containing certain tags
#' ssb_metadata_arbeidskraft <- get_ssb_metadata(data_tag = 'arbeidskraft')
#'
get_ssb_metadata <- function(data_tag = NULL) {
  metadata_url <- paste0(ssb_url, 'v0/dataset/list.csv?lang=no')
  ssb_metadata <- request_ssb(metadata_url)
  ssb_metadata[] <- lapply(ssb_metadata, function(x) if(is.integer(x)) as.character(x) else x)
  if (!is.null(data_tag)){
    ssb_metadata <- subset(ssb_metadata, grepl(data_tag, tags))
    ssb_metadata$table_number <- sapply(strsplit(ssb_metadata$tags, ' '), '[', 1)
  }
  return(ssb_metadata)
}

#' Count SSB metadata tags
#'
#' Count the number of times a tag has been used.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # Get the metadata
#' meta <- get_ssb_metadata()
#' # Count tags
#' tags <- count_ssb_tags(meta)
count_ssb_tags <- function(metadata){
  words_list <- strsplit(metadata$tags, ' ')
  words_df <- data.frame(word = unlist(words_list))
  words_df$tag_count <- ave(seq_along(words_df$word), words_df$word, FUN = length)
  words_df <- words_df[order(-words_df$tag_count),]
  words_df <- words_df[!duplicated(words_df$word),]
  return(words_df)
}

#' Get SSB table
#'
#' Fetch ready-made tables from Statistics Norway (SSB).
#'
#' @details
#' The function returns a ready-made table produced by Statistics Norway, based on the
#' provided table ID. A list of all available IDs can be found with [get_ssb_metadata()].
#'
#' Note that the metadata is updated daily at 05.00 and 11.30 AM, which can lead
#' to the API being unavailable for up to five minutes.
#'
#' See the [API documentation](https://data.ssb.no/api/) for further details.
#'
#' @param id The table ID. Corresponds to the ID variable in the metadata.
#'
#' @return data.frame
#' @export
#' @seealso [get_ssb_metadata()] to view the metadata.
#'
#' @examples
#' # Get table with ID '1104'
#' df <- get_ssb_table('1104')
get_ssb_table <- function(id) {
    table_url <- paste0(ssb_url, 'v0/dataset/', id,'.csv?lang=no')
    ssb_data <- request_ssb(table_url)
    return(ssb_data)
}
