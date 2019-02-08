#' Search database of site names
#' @description Searches the listing of EA monitoring sites to find rows
#' that contain the string provided. Can search by name (\code{name}),
#' Management Catchment (\code{MC}), Operational Catchment (\code{OC})
#' or River Basin District (\code{RBD}). The search is done on a local copy
#' of the waterbody listing rather than connecting to the EA site.
#
#' @param string The search string to be matched. Will match whole or partial
#' strings in the column values.
#'
#' @param column The column to be searched. Possible options are
#' \code{name}, \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District)
#'
#' @return A data frame containing the details of all the sites that match
#' the search string (full or partial matches) in the column specified.
#'
#' @export search_sites
#'
#' @examples
#' # search for Operational Catchments containing "Avon"
#' search_sites("Avon", "OC")
#' 
#' # search for River Basin Districts containing "Tweed"
#' search_sites("Tweed", "RBD")
search_sites <- function(string = NULL, column = NULL) {
  search_choices <- c("name", "MC", "OC", "RBD")
  # if there is a value passed for both arguments
  ################
  ##### this does not catch error if only one passed!
  ################
  #  if (!is.null(column) & !is.null(string)){

  if (is.null(column) | is.null(string)) {
    stop("Both a search string and column (name, MC, OC, or RBD) should be specified", "\n")
  }
  # if the column is found in ea_wbids
  if (column %in% search_choices) {
    # extract list of rows that match search string
    matching_rows <- ea_wbids[grep(string, ea_wbids[, column]), ]
    # remove indexing columns
    matching_rows <- matching_rows[, c(-5, -7, -9)]
  } else {
    stop("Column specified should be: name, MC, OC, or RBD", "\n")
  }
  #  }
  if (nrow(matching_rows) == 0) {
    stop(paste0("No matches found for ", string))
  }
  else {
    return(matching_rows)
  }
} # end of function
