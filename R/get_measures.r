#' Retrieve Measures Specified for Waterbodies 
#' @description Retrieves details of the measures put in place or proposed 
#' for specified waterbodies to try and achieve the status objectives set
#' for 2021 from the EA Catchment Data Explorer site.
#' Data can be retrieved by specifying  Management Catchment (\code{MC}), 
#' Operational Catchment (\code{OC}) or River Basin District (\code{RBD}).
#' Note that Measures data are patchy as only measures explicitly linked 
#' to a target change in status are included. Therefore in many cases 
#' the function will return an empty dataframe with a message.
#
#' @param ea_name A string representing the description (\code{name} for 
#' \code{OC}, \code{MC} or \code{RBD} level downloads or \code{WBID} for 
#' individual waterbodies) of the features to be extracted. For example 
#' to extract data for the whole of the Humber RBD, this would be "Humber"; 
#' also see examples. Must be anexact match to the values used in the EA 
#' database. Use the \code{\link{search_names}} function to search for 
#' specific values.
#'
#' @param column The column to be searched. Possible options are
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District).
#'
#' @return An object of class \code{cde_df} containing the details 
#' of the measures put in place to try and improve water quality.
#' For details of the meaning of the the different columns returned, 
#' see \url{https://ropensci.github.io/cde/articles/cde-output-reference.html}.

#'
#' @export get_measures
#'
#' @examples
#' # get the measures put in place for the Thames RBD
#' get_measures(ea_name="Thames", column="RBD")
#' 
#' # get the measures put in place for the Loddon Operational Catchment
#' get_measures(ea_name="Loddon", column="OC")
#' 
get_measures <- function(ea_name = NULL, column = NULL) {
  # start by running general checks on input data
  check_args(ea_name, column, NULL, NULL, NULL)
  # list of possible columns to select on
  choices <- c("MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices 
         (\"OC\", \"MC\" or \"RBD\").")
  }
  # if all inputs valid, download data
  measures_data <- download_cde(ea_name, column, data_type="measures")
  
  if (nrow(measures_data)==0){
    message("No measures data specified - empty dataframe returned")
  }
  # add comment for plot method identification
  comment(measures_data)<-paste0("measures;",column)
  return(as.cde(measures_data))
} # end of function
