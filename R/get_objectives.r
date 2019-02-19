#' Retrieve Reasons for Not Achieving Good Status 
#' @description Retrieves details of Reasons for Not Achieving Good (RNAG)
#' status from EA Catchment Data Explorer site.
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
#' @param col_value A string representing the description (name) of the
#' features to be extracted. For example to extract data for the whole of
#' the Humber RBD, this would be "Humber"; also see examples. Must be an
#' exact match to the values used in the EA database.
#' Use the \code{\link{search_names}} function to search for specific values.
#'
#' @param column The column to be searched. Possible options are
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District)
#'
#' @param startyr The data can be extracted for specific years using the
#' \code{startyr} and \code{endyr} arguments. If only \code{startyr} is
#' specified this extracts for a particular year. If no years are specified
#' all years are returned.
#'
#' @param endyr The data can be extracted for specific years using the
#' \code{startyr} and \code{endyr} arguments. The \code{endyr} should
#' only be specified if \code{startyr} is also included, otherwise it
#' is ignored and all years are returned.
#'
#' @param type Type of waterbody to be extracted. For Operational/Management
#' catchment level or RBD level queries, the data can also be subset by
#' waterbody type. Possible values are \code{River}, \code{Lake},
#' \code{GroundWaterBody}, \code{TransitionalWater} or \code{CoastalWater}.
#'
#' @return A data frame containing the details of the Reasons for Not 
#' Achieving Good Status for the specified combination of column, value, 
#' dates and type.
#'
#' @export get_objectives
#'
#' @examples
#' # get RNAG issues identified for waterbody GB112071065700
#' \dontrun{get_rnag("GB112071065700", "WBID")}
#' 
#' # get the RNAG issues for Lakes in the Humber RBD, between
#' # 2012 and 2014
#' \dontrun{get_rnag("Humber", "RBD", startyr = 2012, endyr = 2014, type = "Lake")}
#' 
#' # get the RNAG issues for Rivers in the Avon Warwickshire
#' # Operational Catchment in 2011
#' \dontrun{get_rnag("Avon Warwickshire", "MC", startyr = 2011, type = "River")}
get_objectives <- function(col_value = NULL, column = NULL, startyr = NULL, endyr = NULL, type = NULL) {
  # start by running general checks on input data
  check_args(col_value, column, startyr, endyr, type)
  # list of possible columns to select on
  choices <- c("MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices (\"OC\", \"MC\" or \"RBD\").")
  }
  # if all inputs valid, download data
  measures_data <- download_cde(col_value, column, data_type="measures")
  
  # rename columns for consistency with get_status
  names(measures_data)[which(names(measures_data) == "River.Basin.District")] <- "River.basin.district"
  names(measures_data)[which(names(measures_data) == "Management.Catchment")] <- "Management.catchment"
  names(measures_data)[which(names(measures_data) == "Operational.Catchment")] <- "Operational.catchment"
  if (nrow(measures_data)==0){
    message("No measures data specified - empty dataframe returned")
  }
  return(measures_data)
} # end of function
