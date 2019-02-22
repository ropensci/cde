#' Download EA Catchment Data
#' @description Downloads classification data from EA Catchment Data
#' Explorer site. Data can be downloaded by specifying waterbody id
#' (\code{WBID}), Management Catchment (\code{MC}), Operational
#' Catchment (\code{OC}) or River Basin District (\code{RBD}).
#' Start year (\code{startyr}) and end year (\code{endyr}) allow
#' specific timeranges to be downloaded. This is not exported as it is
#' called by \code{\link{get_status}}.
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
#' @param data_type The type of data to be retrieved, either status 
#' classification ("class") or Reasons of Not Acheving Good ("rnag").
#' 
#' @return A data frame containing the classifcation or RNAG details for the
#' specified combination of column and value.
#'
#' @noRd


download_cde <- function(ea_name = NULL, column = NULL, data_type=NULL) {
  # set up url components
  base_url<-"http://environment.data.gov.uk/catchment-planning/"
  # need to set the end URL differently depending on data_type
  if (data_type=="class"){
    end_url<-"/classification?item=all&status=all&format=csv"
  }
  if (data_type=="rnag"){
    end_url<-"/ReasonsForNotAchievingGood?item=all&format=csv"
  }
  if(data_type=="measures"){
    end_url<-"/Action?format=csv"
  }
  if(data_type=="pa"){
    end_url<-"/pa/csv"
  }
  if (data_type=="objectives"){
    end_url<-"/outcome?item=all&status=all&format=csv"
  }

  if (column == "RBD") {
    # rbd level extraction
    index_num <- ea_wbids$RBD.num[which(ea_wbids[, column] == ea_name)][1]
    if (is.na(index_num)) {
      stop("River Basin District name specified not found.")
    } else {
      downloadurl <- paste0(base_url, "RiverBasinDistrict/", index_num, end_url)
      cde_data <- zip_download(downloadurl)
    }
  } # end of rbd extraction
  if (column == "MC") {
    # mc level extraction
    index_num <- ea_wbids$MC.num[which(ea_wbids[, column] == ea_name)][1]
    if (is.na(index_num)) {
      stop("Management Catchment name specified not found.")
    } else {
      downloadurl <- paste0(base_url, "ManagementCatchment/", index_num, end_url)
      cde_data <- zip_download(downloadurl)
    }
  } # end of mc extraction
  # oc next
  if (column == "OC") {
    # oc level extraction - works
    index_num <- ea_wbids$OC.num[which(ea_wbids[, column] == ea_name)][1]
    if (is.na(index_num)) {
      stop("Operational catchment name specified not found.")
    } else {
      cde_data <- data.table::fread(paste0(base_url, "OperationalCatchment/", index_num, end_url), showProgress = FALSE, header = TRUE, stringsAsFactors = FALSE, check.names=TRUE, data.table=FALSE)
    }
  } # end of oc extraction
  # finally wbid
  if (column == "WBID") {
    # wbid level extraction
    if (ea_name %in% ea_wbids[, "WBID"]) {
      if (data_type=="rnag"){
        # have to add supress warnings as data.table does not like empty RNAG data (bad download format on the part of EA)
        suppressWarnings(cde_data <- data.table::fread(paste0(base_url, "data/reason-for-failure.csv?waterBody=", ea_name, "&_view=csv"), showProgress = FALSE, header = TRUE, stringsAsFactors = FALSE, check.names=TRUE, data.table=FALSE))
      }
      if (data_type=="objectives"){
        cde_data <- data.table::fread(paste0(base_url, "so/WaterBody/", ea_name, "/objective-outcomes.csv?_view=csv"), showProgress = FALSE, header = TRUE, stringsAsFactors = FALSE, check.names=TRUE, data.table=FALSE)
      }
      if (data_type=="pa"){
        cde_data <- data.table::fread(paste0(base_url, "WaterBody/", ea_name, "/pa/csv"), showProgress = FALSE, header = TRUE, stringsAsFactors = FALSE, check.names=TRUE, data.table=FALSE)
      }
      if (data_type=="class"){
        cde_data <- data.table::fread(paste0(base_url, "data/classification.csv?waterBody=", ea_name, "&_view=csv"), showProgress = FALSE, header = TRUE, stringsAsFactors = FALSE, check.names=TRUE, data.table=FALSE)
      }
    }
    else {
      stop("WBID value specified not found.")
    }
  }
  # end of wbid extraction
  return(cde_data)
} # end of function

#' Download Zipfile and extract csv
#' @description Downloads zipfile from specified url, unzips to
#' csv file and reads csv into dataframe.
#
#' @param download_url A string representing the url to download the
#' zip file from.
#' 
#' @noRd

zip_download <- function(download_url) {
  temp <- tempfile()
  curl::curl_download(download_url, temp, mode = "wb")
  # extract data from zipfile to df using data.table to speed things up
  csvfile <- utils::unzip(temp, junkpaths = TRUE)
  catchment_data <- data.table::fread(csvfile, stringsAsFactors = FALSE, check.names = TRUE, data.table = FALSE, showProgress = FALSE)
  # delete the intermediate files
  unlink(temp)
  unlink(csvfile)
  return(catchment_data)
} # end of function


#' Check common arguments to functions
#' @description Checks the ea_name, year ranges and waterbody type
#' for all functions 
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
#' @noRd

check_args <- function(ea_name = NULL, column = NULL, startyr = NULL, endyr = NULL, type = NULL) {
   # check that both ea_name and column are present
  if (is.null(ea_name) | is.null(column)) {
    stop("Both ea_name (name) and column (\"WBID\", \"MC\", \"OC\", or \"RBD\") should be specified", "\n")
  }
  # are years, if present, numeric?
  if (!is.null(startyr) & !is.null(endyr)) {
    if (!is.numeric(startyr) | !is.numeric(endyr)) {
      stop("Please enter numeric values for the starting and ending years")
    }
  }
  # if there is a startyr set
  if (!is.null(startyr)) {
    if (startyr < 2009) {
      stop("Starting year cannot be before 2009")
    }
    # if there is an end year alsoset
    if (!is.null(endyr)) {
      # check values make sense
      if (!endyr >= startyr) {
        stop("End year is before start year: please correct.")
      }
    }
  }
  # catch when only endyr is set
  if (is.null(startyr) & !is.null(endyr)){
    stop("Only end year specified, also needs start year.")
  }
  # check that the waterbody type is a valid choice
  if (!is.null(type)) {
    types <- c("River", "Lake", "TransitionalWater", "GroundWaterBody", "CoastalWater")
    if (!type %in% types) {
      stop("Type specified is not a valid choice (\"River\", \"Lake\", \"CoastalWater\", \"TransitionalWater\" or \"GroundWaterBody\"")
    }
  }
}
# end of function

#' Subset data as required
#' @description Subsets data by year, year range, classification level
#' and waterbody type as required
#' 
#' @param full_data The dataframe to be subset
#' 
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
#' extracted. Defaults to 'Overall Water Body'. See docs for possible values.
#'
#' @param startyr The data can be extracted for specific years using the
#' \code{startyr} and \code{endyr} arguments. If only \code{startyr} is
#' specified this extracts for a particular year. If no years are specified
#' all years are returned.
#'
#' @param endyr The data can be extracted for specific years using the
#' \code{startyr} and \code{endyr} arguments. The \code{endyr} should
#' only be specified if \code{startyr} is also included.
#'
#' @param type Type of waterbody to be extracted. For Operational/Management
#' catchment level or RBD level queries, the data can also be subset by
#' waterbody type. Possible values are \code{River}, \code{Lake},
#' \code{GroundWaterBody}, \code{TransitionalWater} or \code{CoastalWater}.
#'
#' @return A data frame that has been subsetted by the 
#' specified combination of column, value, level and dates.
#'
#' @noRd
#'
subset_data <- function(full_data, ea_name = NULL, column = NULL, level = "Overall Water Body", startyr = NULL, endyr = NULL, type = NULL) {

  # if only start year is set, is it beyond the data range?
  if (!is.null(startyr) & is.null(endyr)){
    if (startyr>max(full_data$Year)){
      message(paste0("Start year is beyond the most recent year of data (",max(full_data$Year),")"))
      message("Just outputting most recent year")
      startyr<-max(full_data$Year)
    }
  }
  # if endyr is set, is it beyond the data range?
  if (!is.null(endyr)){
    if (endyr>max(full_data$Year)){
      message(paste0("End year is beyond the most recent year of data (",max(full_data$Year),")"))
      message("Subsetting to most recent year")
      endyr<-max(full_data$Year)
    }
  }
  # if they are both set, check the endyr
  if (!is.null(startyr) & !is.null(endyr)) {
    if (endyr>max(full_data$Year)){
      message(paste0("End year is beyond the most recent year of data (",max(full_data$Year),")"))
      message("Subsetting to most recent year")
      endyr<-max(full_data$Year)
    }
    # if both years are specified, subset by range
    full_data <- full_data[full_data$Year >= startyr & full_data$Year <= endyr, ]
  }
  else if (!is.null(startyr)) {
    full_data <- full_data[full_data$Year == startyr, ]
  }
  # level subsetting, defaults to "Overall Water Body"
  # for Chemical and Supporting Elements levels, need to deal with options for
  # surface waters and groundwaters
  if (!is.null(level)){
    if (level == "Chemical") {
      full_data <- full_data[full_data$Classification.Item == "Chemical" | full_data$Classification.Item == "Chemical (GW)", ]
    }
    else if (level == "Supporting elements") {
      full_data <- full_data[full_data$Classification.Item == "Supporting elements (Surface Water)" | full_data$Classification.Item == "Supporting elements (Groundwater)", ]
    }
    else {
      full_data <- full_data[full_data$Classification.Item == level, ]
    }
  }
  # now Water.body.type
  if (!is.null(type)) {
    full_data <- full_data[full_data$Water.body.type == type, ]
  }
  # if year range covers 2013 and 2014, subset to just include cycle 2 data
  # avoids double counting of waterbodies
  full_data <- full_data[!(full_data$Year == 2013 & full_data$Cycle == 1 | full_data$Year == 2014 & full_data$Cycle == 1), ]
  
  return(full_data)
} # end of function