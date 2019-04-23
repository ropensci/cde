#' Retrieve WFD Status Classification Data
#' @description Retrieves WFD Status classification data from EA Catchment 
#' Data Explorer site. Data can be retrieved by specifying waterbody id
#' (\code{WBID}), Management Catchment (\code{MC}), Operational
#' Catchment (\code{OC}) or River Basin District (\code{RBD}).
#' Start year (\code{startyr}) and end year (\code{endyr}) allow
#' specific timeranges to be downloaded.
#' For Management Catchment (\code{MC}), Operational
#' Catchment (\code{OC}) or River Basin District (\code{RBD}) level
#' downloads, waterbody \code{type} can also be specified to allow
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
#' @param level The level within the WFD quality status classification to be
#' extracted. Defaults to 'Overall Water Body'. See Vignette for possible 
#' values.
#'
#' @param startyr The data can be extracted for specific years using the
#' \code{startyr} and \code{endyr} arguments. If only \code{startyr} is
#' specified this extracts for a particular year. If no years are specified
#' all years are returned.
#'
#' @param endyr The data can be extracted for specific years using the
#' \code{startyr} and \code{endyr} arguments. The \code{endyr} should
#' only be specified if \code{startyr} is also included, otherwise an 
#' error is returned.
#'
#' @param type Type of waterbody to be extracted. For Operational/Management
#' catchment level or RBD level queries, the data can also be subset by
#' waterbody type. Possible values are \code{River}, \code{Lake},
#' \code{GroundWaterBody}, \code{TransitionalWater} or \code{CoastalWater}.
#'
#' @return A data frame containing the classification details for the
#' specified combination of criteria.
#'
#' @export get_status
#'
#' @examples
#' # get Overall Water Body status classification for waterbody GB520804714300
#' \dontrun{get_status("GB520804714300", "WBID")}
#' 
#' # get status class based on Priority substances for waterbody GB520804714300
#' \dontrun{get_status("GB520804714300", "WBID", level = "Priority substances")}
#' 
#' # get the Overall Water Body status of Lakes in the Humber RBD, between
#' # 2012 and 2014
#' \dontrun{get_status("Humber", "RBD", startyr = 2012, endyr = 2014, type = "Lake")}
#' 
#' # get the Overall Water Body status for Rivers in the Avon Warwickshire
#' # Operational Catchment in 2011
#' \dontrun{get_status("Avon Warwickshire", "MC", startyr = 2011, type = "River")}
#' 
get_status <- function(ea_name = NULL, column = NULL, 
    level = "Overall Water Body", startyr = NULL, endyr = NULL, type = NULL) {
  # start by running general checks on input data
  check_args(ea_name, column, startyr, endyr, type)
  # list of possible columns to select on
  choices <- c("WBID", "MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices 
         (\"WBID\", \"OC\", \"MC\" or \"RBD\").")
  }
  # list of classification levels that can be extracted
  class_levels <- c("Overall Water Body", "Ecological", "Chemical", 
      "Quantitative", "Biological quality elements", 
      "Hydromorphological Supporting Elements", 
      "Physico-chemical quality elements", "Specific pollutants",
      "Priority hazardous substances", "Priority substances", 
      "Quantitative Status element", "Chemical Status element", 
      "Supporting elements", "Other Substances")
  if (!level %in% class_levels) {
    stop(paste0("Classification level specified: ", level, 
        ", is not a valid choice"))
  }
  # if WB level download, type should not be specified, so give message
  if (column=="WBID" & !is.null(type)){
    type<-NULL
    message("Type is ignored for WBID objectives")
  }
  # if all inputs valid, download data
  status_data <- download_cde(ea_name, column, "class")

  # check if any data returned
  if (nrow(status_data)==0){
    message("No status data for combination specified - 
      empty dataframe returned")
    return(as.cde(status_data))
  }else{
    # subset data as required
    status_data<-subset_data(status_data, column, level, startyr, endyr, type)
    if (nrow(status_data)==0){
      message("No status data for combination specified - 
        empty dataframe returned")
    }
  }
  # add comment for plot method identification
  comment(status_data) <- "status"
  return(as.cde(status_data))
} # end of function
