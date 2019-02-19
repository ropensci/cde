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
#' @param data_type The type of data to be retrieved, either status 
#' classification ("class") or Reasons of Not Acheving Good ("rnag").
#' 
#' @return A data frame containing the classifcation or RNAG details for the
#' specified combination of column and value.
#'
#' @noRd


download_cde <- function(col_value = NULL, column = NULL, data_type=NULL) {
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

    # note different for wb 
  # list of possible columns to select on
  ########### DONT NEED THIS ########
  choices <- c("WBID", "MC", "OC", "RBD")
  # is a value/column specified
  if (!is.null(column) & !is.null(col_value)) {
    # is the column specified correctly
    if (column %in% choices) {
      if (column == "RBD") {
        # rbd level extraction
        index_num <- ea_wbids$RBD.num[which(ea_wbids[, column] == col_value)][1]
        if (is.na(index_num)) {
          stop("River Basin District name specified not found.")
        } else {
          downloadurl <- paste0(base_url, "RiverBasinDistrict/", index_num, end_url)
          cde_data <- zip_download(downloadurl)
        }
      } # end of rbd extraction
      else if (column == "MC") {
        # rbd level extraction
        index_num <- ea_wbids$MC.num[which(ea_wbids[, column] == col_value)][1]
        if (is.na(index_num)) {
          stop("Management Catchment name specified not found.")
        } else {
          downloadurl <- paste0(base_url, "ManagementCatchment/", index_num, end_url)
          cde_data <- zip_download(downloadurl)
        }
      } # end of mc extraction
      # oc next
      else if (column == "OC") {
        # oc level extraction - works
        index_num <- ea_wbids$OC.num[which(ea_wbids[, column] == col_value)][1]
        if (is.na(index_num)) {
          stop("Operational catchment name specified not found.")
        } else {
          cde_data <- utils::read.csv(paste0(base_url, "OperationalCatchment/", index_num, end_url), header = TRUE, stringsAsFactors = FALSE)
        }
      } # end of oc extraction
      # finally wbid
      else if (column == "WBID") {
        # wbid level extraction
        if (col_value %in% ea_wbids[, "WBID"]) {
          if (data_type=="rnag"){
            cde_data <- utils::read.csv(paste0(base_url, "data/reason-for-failure.csv?waterBody=", col_value, "&_view=csv"), header = TRUE, stringsAsFactors = FALSE)
          }
          if (data_type=="objectives"){
            cde_data <- utils::read.csv(paste0(base_url, "so/WaterBody/", col_value, "/objective-outcomes.csv?_view=csv"), header = TRUE, stringsAsFactors = FALSE)
            }else{
            cde_data <- utils::read.csv(paste0(base_url, "WaterBody/", col_value, "/csv"), header = TRUE, stringsAsFactors = FALSE)
          }
        }
        else {
          stop("WBID value specified not found.")
        }
      } # end of wbid extraction
    } else {
      stop("Column specified should be one of \"WBID\", \"MC\", \"OC\" or \"RBD\"")
    }
  }
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
  catchment_data <- data.table::fread(csvfile, stringsAsFactors = FALSE, check.names = TRUE, data.table = FALSE)
  # delete the intermediate files
  unlink(temp)
  unlink(csvfile)
  return(catchment_data)
} # end of function


#' Check common arguments to functions
#' @description Checks the col_value, year ranges and waterbody type
#' for all functions 
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
#' @noRd

check_args <- function(col_value = NULL, column = NULL, startyr = NULL, endyr = NULL, type = NULL) {
   # check that both col_value and column are present
  if (is.null(col_value) | is.null(column)) {
    stop("Both col_value (name) and column (\"WBID\", \"MC\", \"OC\", or \"RBD\") should be specified", "\n")
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
        stop("End year is before Start year: please correct.")
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