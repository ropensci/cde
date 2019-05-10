#' Retrieve Protected Area Information 
#' @description Retrieves details of Protected Areas associated with 
#' waterbodies, catchments or River Basin Districts from the EA 
#' Catchment Data Explorer site.
#' Data can be retrieved by specifying waterbody id
#' (\code{WBID}), Management Catchment (\code{MC}), Operational
#' Catchment (\code{OC}) or River Basin District (\code{RBD}).
#
#' @param ea_name A string representing the description (\code{name} for 
#' \code{OC}, \code{MC} or \code{RBD} level downloads or \code{WBID} for 
#' individual waterbodies) of the features to be extracted. For example 
#' to extract data for the whole of the Humber RBD, this would be "Humber"; 
#' also see examples. Must be an exact match to the values used in the 
#' EA database. Use the \code{\link{search_names}} function to search 
#' for specific values.
#'
#' @param column The column to be searched. Possible options are
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District)
#'
#' @return An object of class \code{cde_df} containing the details of the 
#' Protected Areas associated with the waterbodies.
#' For details of the meaning of the the different columns returned, 
#' see \url{https://robbriers.github.io/cde/articles/cde-output-reference.html}.

#'
#' @export get_pa
#'
#' @examples
#' # get protected areas associated with waterbody GB112071065700
#' get_pa("GB112071065700", "WBID")
#' 
#' # get the protected areas associated with the Humber RBD
#' get_pa("Humber", "RBD")
#' 
#' # get the protected areas associated with the Avon Warwickshire
#' # Management Catchment
#' get_pa("Avon Warwickshire", "MC")
#' 
get_pa <- function(ea_name = NULL, column = NULL) {
  # start by running general checks on input data, setting years to NULL
  check_args(ea_name, column, NULL, NULL, NULL)
  # list of possible columns to select on
  choices <- c("WBID", "MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices 
         (\"WBID\", \"OC\", \"MC\" or \"RBD\").")
  }
  # if all inputs valid, download data
  pa_data <- download_cde(ea_name, column, data_type="pa")
  
  if (nrow(pa_data)==0){
    message("No protected areas present - empty dataframe returned")
  }
  # add comment for plot method identification
  comment(pa_data) <- paste0("pa;", column)
  return(as.cde(pa_data))
} # end of function
