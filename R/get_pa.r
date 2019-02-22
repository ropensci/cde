#' Retrieve Protected Area Information 
#' @description Retrieves details of Protected Areas associated with 
#' waterbodies, catchments or River Basin Districts from the EA 
#' Catchment Data Explorer site.
#' Data can be retrieved by specifying waterbody id
#' (\code{WBID}), Management Catchment (\code{MC}), Operational
#' Catchment (\code{OC}) or River Basin District (\code{RBD}).
#' Start year (\code{startyr}) and end year (\code{endyr}) allow
#' specific timeranges to be downloaded.
#' For Management Catchment (\code{MC}), Operational
#' Catchment (\code{OC}) or River Basin District (\code{RBD}) level
#' downloads, waterbody \code{Type} can also be specified to allow
#' extraction of specific waterbody types (River, Lake etc).
#
#' @param ea_name A string representing the description (name) of the
#' features to be extracted. For example to extract data for the whole of
#' the Humber RBD, this would be "Humber"; also see examples. Must be an
#' exact match to the values used in the EA database.
#' Use the \code{\link{search_names}} function to search for specific values.
#'
#' @param column The column to be searched. Possible options are
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District)
#'
#' @return A data frame containing the details of the Protected Areas 
#' associated with the waterbodies.
#'
#' @export get_pa
#'
#' @examples
#' # get protected areas associated with waterbody GB112071065700
#' \dontrun{get_pa("GB112071065700", "WBID")}
#' 
#' # get the protected areas associated with Lakes in the Humber RBD
#' \dontrun{get_pa("Humber", "RBD", type = "Lake")}
#' 
#' # get the protected areas associated with Rivers in the Avon Warwickshire
#' # Operational Catchment
#' \dontrun{get_pa("Avon Warwickshire", "MC", type = "River")}
#' 
get_pa <- function(ea_name = NULL, column = NULL) {
  # start by running general checks on input data, setting years to NULL
  check_args(ea_name, column, NULL, NULL, NULL)
  # list of possible columns to select on
  choices <- c("WBID", "MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices (\"WBID\", \"OC\", \"MC\" or \"RBD\").")
  }
  # if all inputs valid, download data
  pa_data <- download_cde(ea_name, column, data_type="pa")
  
  # rename columns for consistency with get_status
  names(pa_data)[which(names(pa_data) == "River.Basin.District")] <- "River.basin.district"
  names(pa_data)[which(names(pa_data) == "Management.Catchment")] <- "Management.catchment"
  names(pa_data)[which(names(pa_data) == "Operational.Catchment")] <- "Operational.catchment"
  if (nrow(pa_data)==0){
    message("No protected areas present - empty dataframe returned")
  }
  return(pa_data)
} # end of function
