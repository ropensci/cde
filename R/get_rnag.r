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
#' @export get_rnag
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
get_rnag <- function(col_value = NULL, column = NULL, startyr = NULL, endyr = NULL, type = NULL) {
  # start by running general checks on input data
  check_args(col_value, column, startyr, endyr, type)
  # list of possible columns to select on
  choices <- c("WBID", "MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices (WBID, OC, MC or RBD).")
  }
  # if all inputs valid, download data
  rnag_data <- download_cde(col_value, column, data_type="rnag")
  # rename columns for consistency with get_status
  if (column=="WBID"){
    names(rnag_data)[which(names(rnag_data) == "water.body.type")] <- "Water.body.type"
    names(rnag_data)[which(names(rnag_data) == "River.Basin.District")] <- "River.basin.district"
    names(rnag_data)[which(names(rnag_data) == "Management.Catchment")] <- "Management.catchment"
    names(rnag_data)[which(names(rnag_data) == "Operational.Catchment")] <- "Operational.catchment"
  }
  if (column!="WBID"){
    names(rnag_data)[which(names(rnag_data) == "Water.body.id")] <- "Waterbody.ID"
    names(rnag_data)[which(names(rnag_data) == "Classification.Year")] <- "Year"
    names(rnag_data)[which(names(rnag_data) == "Classification.Status")] <- "status"
    names(rnag_data)[which(names(rnag_data) == "River.Basin.District")] <- "River.basin.district"
    names(rnag_data)[which(names(rnag_data) == "Management.Catchment")] <- "Management.catchment"
    names(rnag_data)[which(names(rnag_data) == "Operational.Catchment")] <- "Operational.catchment"
  }
  # do subsetting here - years first
  # if only start year is set, is it beyond the data range?
  if (!is.null(startyr) & is.null(endyr)){
    if (startyr>max(rnag_data$Year)){
      message(paste0("Start year is beyond the most recent year of data (",max(rnag_data$Year),")"))
      message("Just outputting most recent year")
      startyr<-max(rnag_data$Year)
    }
  }
  # if endyr is set, is it beyond the data range?
  if (!is.null(endyr)){
    if (endyr>max(rnag_data$Year)){
      message(paste0("End year is beyond the most recent year of data (",max(rnag_data$Year),")"))
      message("Subsetting to most recent year")
      endyr<-max(rnag_data$Year)
    }
  }
  # if they are both set, check the endyr
  if (!is.null(startyr) & !is.null(endyr)) {
    if (endyr>max(rnag_data$Year)){
      message(paste0("End year is beyond the most recent year of data (",max(rnag_data$Year),")"))
      message("Subsetting to most recent year")
      endyr<-max(rnag_data$Year)
    }
  # if both years are specified, subset by range
    rnag_data <- rnag_data[rnag_data$Year >= startyr & rnag_data$Year <= endyr, ]
  }
  else if (!is.null(startyr)) {
    rnag_data <- rnag_data[rnag_data$Year == startyr, ]
  }
  # subset by Water.body.type
  if (!is.null(type)) {
    rnag_data <- rnag_data[rnag_data$Water.body.type == type, ]
  }
  # if year range covers 2013 and 2014, subset to just include cycle 2 data
  # avoids double counting of waterbodies
  rnag_data <- rnag_data[!(rnag_data$Year == 2013 & rnag_data$Cycle == 1 | rnag_data$Year == 2014 & rnag_data$Cycle == 1), ]
  return(rnag_data)
} # end of function