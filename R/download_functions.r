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

#' @return A data frame containing the classifcation details for the
#' specified combination of column and value.
#'

download_ea <- function(col_value = NULL, column = NULL) {
  # set up url components
  base_url<-"http://environment.data.gov.uk/catchment-planning/"
  end_url<-"/classification?item=all&status=all&format=csv"
  # list of possible columns to select on
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
          classifications <- zip_download(downloadurl)
        }
      } # end of rbd extraction
      else if (column == "MC") {
        # rbd level extraction
        index_num <- ea_wbids$MC.num[which(ea_wbids[, column] == col_value)][1]
        if (is.na(index_num)) {
          stop("Management Catchment name specified not found.")
        } else {
          downloadurl <- paste0(base_url, "ManagementCatchment/", index_num, end_url)
          classifications <- zip_download(downloadurl)
        }
      } # end of mc extraction
      # oc next
      else if (column == "OC") {
        # oc level extraction - works
        index_num <- ea_wbids$OC.num[which(ea_wbids[, column] == col_value)][1]
        if (is.na(index_num)) {
          stop("Operational catchment name specified not found.")
        } else {
          classifications <- utils::read.csv(paste0(base_url, "OperationalCatchment/", index_num, end_url), header = TRUE, stringsAsFactors = FALSE)
        }
      } # end of oc extraction
      # finally wbid
      else if (column == "WBID") {
        # wbid level extraction
        if (col_value %in% ea_wbids[, "WBID"]) {
          classifications <- utils::read.csv(paste0(base_url, "WaterBody/", col_value, "/csv"), header = TRUE, stringsAsFactors = FALSE)
        }
        else {
          stop("WBID value specified not found.")
        }
      } # end of wbid extraction
    } else {
      stop("Column specified should be one of WBID, MC, OC or RBD")
    }
  }
} # end of function

#' Download Zipfile and extract csv
#' @description Downloads zipfile from specified url, unzips to
#' csv file and reads csv into dataframe.
#
#' @param download_url A string representing the url to download the
#' zip file from.

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
