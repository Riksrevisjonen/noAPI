#' extract_table
#' @noRd
extract_table <- function(url){
  resp <- tryCatch({
    GET(url, config(ssl_verifypeer = 0L))
  }, error = function(e) {
    message("Error in GET request: ", e)
    return(NULL)
  })

  content <- httr::content(resp, "text")

  data <- read.csv2(text = content, dec = ",")

  return(data)
}

#' clean_table
#' @noRd
clean_table <- function(df) {

  column_names <- colnames(df)

  # Convert to lower case
  column_names <- tolower(column_names)

  # Remove leading or trailing spaces
  column_names <- trimws(column_names)

  # Replace non-alphanumeric characters with empty string
  column_names <- gsub("[^[:alnum:]_]", " ", column_names)

  # Remove leading digits
  column_names <- gsub("^\\d+", "", column_names)

  # Replace empty column names with "unnamed"
  column_names[column_names == ""] <- "unnamed"

  # Replace spaces with underscores
  column_names <- gsub(" ", "_", column_names)

  # Assign the cleaned names back to the data frame
  colnames(df) <- column_names

  # Return the data frame
  return(df)
}
