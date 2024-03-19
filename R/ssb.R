#' Get SSB table meta data
#'
#' Fetch meta data of ready-made tables from Statistics Norway (SSB).
#'
#' @details
#' The function returns a data.frame with an overview (meta data) of all the available ready-made
#' tables produced by Statistics Norway.
#'
#' Note that the meta data is updated daily at 05.00 and 11.30 AM, which can lead to all tables
#' being unavailable for up to five minutes.
#'
#' See the [API documentation](https://data.ssb.no/api/) for further details (in Norwegian only).
#'
#' @seealso [get_ssb_metadata_tags()] to view all meta data tags, and [get_ssb_table()]
#'   to retrieve the actual tables.
#' @return data.frame
#' @rdname get_ssb_metadata
#' @export
#' @examples
#' # Get data frame of all meta data
#' ssb_metadata <- get_ssb_metadata()
#'
#' # Get a subset of meta data which contains certain tags
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

#' Get tags from meta data
#'
#' Fetch av overview of all tags used in the meta data
#'
#' @seealso [get_ssb_metadata()] to view all meta data, and [get_ssb_table()]
#'   to retrieve the actual tables.
#' @return data.frame
#' @rdname get_ssb_metadata
#' @export
#'
#' @examples
#' # Get data frame of all tags used in the meta data, along with the number of times the tag has been used.
#' tag_list <- get_ssb_metadata_tags()
get_ssb_metadata_tags <- function(){

  ssb_data <- get_ssb_metadata()

  words_list <- strsplit(ssb_data$tags, ' ')

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
#' The function returns a ready-made table produced by Statistics Norway, based on a
#' provided table ID. A list of all available IDs can be found by running
#' the function [get_ssb_metadata()].
#'
#' Note that the meta data is updated daily at 05.00 and 11.30 AM, which can lead to all tables
#' being unavailable for up to five minutes.
#'
#' See the [API documentation](https://data.ssb.no/api/) for further details (in Norwegian only).
#'
#' @param id The table ID corresponding with the ID variable in the meta data.
#'
#' @return data.frame
#' @export
#'
#' @examples
#' # Get data frame of all meta data
#' ssb_metadata <- get_ssb_metadata()
#'
#' # Get data frame of table with ID '1104'
#' table_1104 <- get_ssb_table('1104')
get_ssb_table <- function(id) {
    table_url <- paste0(ssb_url, 'v0/dataset/', id,'.csv?lang=no')

    ssb_data <- request_ssb(table_url)

    return(ssb_data)
  }
