#' Retrieve WFD Status Information
#' @description Retrieves WFD Status class data from EA Catchment Data
#' Explorer site. Data can be retrieved by specifying waterbody id
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
#' @param level The level within the WFD quality status classification to be
#' extracted. Defaults to 'Overall Water Body'. See docs for possible values.
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
#' @return A data frame containing the classifcation details for the
#' specified combination of column, value, level and dates.
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
get_status <- function(col_value = NULL, column = NULL, level = "Overall Water Body", startyr = NULL, endyr = NULL, type = NULL) {
  # start by running general checks on input data
  check_args(col_value, column, startyr, endyr, type)
  # list of possible columns to select on
  choices <- c("WBID", "MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices (WBID, OC, MC or RBD).")
  }
  # list of classification levels that can be extracted
  class_levels <- c("Overall Water Body", "Ecological", "Chemical", "Quantitative", "Biological quality elements", "Hydromorphological Supporting Elements", "Physico-chemical quality elements", "Specific pollutants", "Priority hazardous substances", "Priority substances", "Quantitative Status element", "Chemical Status element", "Supporting elements", "Other Substances")
  if (!level %in% class_levels) {
    stop(paste0("Classification level specified: ", level, ", is not a valid choice"))
  }
  # if all inputs valid, download data
  status_data <- download_ea(col_value, column)

  # do subsetting here - years first
  if (!is.null(startyr) & !is.null(endyr)) {
    # if both years are specified, subset by range
    status_data <- status_data[status_data$Year >= startyr & status_data$Year <= endyr, ]
  }
  else if (!is.null(startyr)) {
    status_data <- status_data[status_data$Year == startyr, ]
  }
  # level subsetting, defaults to "Overall Water Body"
  # for Chemical and Supporting Elements levels, need to deal with options for
  # surface waters and groundwaters
  if (level == "Chemical") {
    status_data <- status_data[status_data$Classification.Item == "Chemical" | status_data$Classification.Item == "Chemical (GW)", ]
  }
  else if (level == "Supporting elements") {
    status_data <- status_data[status_data$Classification.Item == "Supporting elements (Surface Water)" | status_data$Classification.Item == "Supporting elements (Groundwater)", ]
  }
  else {
    status_data <- status_data[status_data$Classification.Item == level, ]
  }
  # now Water.body.type
  if (!is.null(type)) {
    status_data <- status_data[status_data$Water.body.type == type, ]
  }
  # if year range covers 2013 and 2014, subset to just include cycle 2 data
  # avoids double counting of waterbodies
  status_data <- status_data[!(status_data$Year == 2013 & status_data$Cycle == 1 | status_data$Year == 2014 & status_data$Cycle == 1), ]

  # remove web link column - not really needed
  status_data <- status_data[, !(names(status_data) %in% c("classification.ID"))]

  return(status_data)
} # end of function
