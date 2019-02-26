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
#' @param ea_name A string representing the description (name) of the
#' features to be extracted. For example to extract data for the whole of
#' the Humber RBD, this would be "Humber"; also see examples. Must be an
#' exact match to the values used in the EA database.
#' Use the \code{\link{search_names}} function to search for specific values.
#'
#' @param column The column to be searched. Possible options are
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District).
#'
#' @return A data frame containing the details of the measures put in place 
#' to try and improve water quality.
#'
#' @export get_measures
#'
#' @examples
#' # get the measures put in place for the Thames RBD
#' \dontrun{get_measures("Thames", "RBD")}
#' 
#' # get the measures put in place for the Loddon Operational Catchment
#' \dontrun{get_measures("Loddon", "OC")}
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
  
  # rename columns for consistency with get_status
  names(measures_data)[which(names(measures_data) == 
      "River.Basin.District")] <- "River.basin.district"
  names(measures_data)[which(names(measures_data) == 
      "Management.Catchment")] <- "Management.catchment"
  names(measures_data)[which(names(measures_data) == 
      "Operational.Catchment")] <- "Operational.catchment"
  if (nrow(measures_data)==0){
    message("No measures data specified - empty dataframe returned")
  }
  return(measures_data)
} # end of function
