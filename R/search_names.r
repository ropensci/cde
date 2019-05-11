#' Search database of site names
#' @description Searches the listing of EA monitoring sites to find rows
#' that contain the string provided. Can search by WBID (\code{WBID}), name 
#' (\code{name}), Management Catchment (\code{MC}), Operational Catchment 
#' (\code{OC}) or River Basin District (\code{RBD}). There is a hierarchical 
#' relationship between these levels as shown at \url{https://environment.data.gov.uk/catchment-planning/help#help-catchment-hierarchy}.
#' 
#' The search is done on a local copy of the waterbody listing contained in 
#' the \code{\link{ea_wbids}} object rather than connecting to the 
#' EA site.
#
#' @param string The search string to be matched (case-sensitive). Will match 
#' whole or partial strings in the column values.
#'
#' @param column The column to be searched. Possible options are
#' \code{WBID}, \code{name}, \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District)
#'
#' @return A data frame containing the details of all the sites that match
#' the search string (full or partial matches) in the column specified.
#' Columns returned are defined in \code{\link{ea_wbids}}.
#'
#' @export search_names
#'
#' @examples
#' # search for sites containing "Tadnoll" in the name
#' search_names(string="Tadnoll", column="name")
#' 
#' # search for Operational Catchments containing "Cornwall"
#' search_names(string="Cornwall", column="OC")
#' 
search_names <- function(string = NULL, column = NULL) {
  search_choices <- c("name","WBID", "MC", "OC", "RBD")
  # check if there is a value passed for both arguments
  if (is.null(column) | is.null(string)) {
    stop("Both a search string and column (name, WBID, MC, OC, or RBD) 
         should be specified", "\n")
  }
  # if the column is found in ea_wbids
  if (column %in% search_choices) {
    # extract list of rows that match search string
    matching_rows <- ea_wbids[grep(string, ea_wbids[, column]), ]
    # remove indexing columns
    matching_rows <- matching_rows[, c(-5, -7, -9)]
  } else {
    stop("Column specified should be: name, WBID, MC, OC, or RBD", "\n")
  }
  #  }
  if (nrow(matching_rows) == 0) {
    stop(paste0("No matches found for ", string))
  }
  else {
    return(matching_rows)
  }
} # end of function
