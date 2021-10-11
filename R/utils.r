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
#' classification ("class"), Reasons of Not Acheving Good ("rnag"), 
#' objectives ("objectives"), measures ("measures") or Protected 
#' Areas ("pa").
#' 
#' @return An object of class \code{cde_df}  containing the classifcation, 
#' RNAG, measures, objectives or protected areas for the specified 
#' combination of column and value.
#'
#' @noRd


download_cde <- function(ea_name = NULL, column = NULL, data_type=NULL) {
  # do search to make sure that name is present
  search_names(ea_name, column)

  # this gives either index number (RBD, MC, OC) or wbid for next bit
  index<-find_index(column, ea_name)
  # do download using either zip or plain fread depending on type
  ############################################# removing zip dls ###############
  ### if (column=="RBD" | column=="MC"){
  #  cde_data <- zip_download(set_url(column, data_type, index))
  #} else{
    cde_data <- tryCatch(data.table::fread(set_url(column, data_type, index),
                    showProgress = TRUE, header = TRUE, stringsAsFactors = FALSE, 
                    check.names=TRUE, data.table=FALSE),
                    warning=function(w){})
  ###########################################################}
  # substitute . for _ in all column names
  names(cde_data)<-gsub(".", "_", names(cde_data), fixed=TRUE)
  # convert all to lower case
  names(cde_data)<-tolower(names(cde_data))
  return(cde_data)
} # end of function

#' Set end URL
#' @description Sets the final part of the download URL to the correct
#' string depending on data type to be downloaded (except for WBIDs).
#
#' @param data_type A string representing the type of data (class, rnag, 
#' measures, pa or objectives) to be downloaded.
#' 
#' @noRd

set_end_url<-function(data_type){
  switch(data_type, 
         #         "class" = "/classification?item=all&status=all&format=csv",

         "class" = "/classifications.csv",
         "rnag" = "/rnags.csv",
         # not sure there are any measures info available now
         #"measures" = "/Action?format=csv",
         "pa" = "/protected-areas.csv",
         "objectives" = "/objectives.csv")
}

#' Set end URL for WBID level downloads (different format to rest)
#' @description Sets the final part of the download URL to the correct
#' string depending on data type to be downloaded for WBID level downloads.
#
#' @param data_type A string representing the type of data (class, rnag, 
#' measures, objectives or pa) to be downloaded.
#' 
#' @param index Either an integer representing the catchment number on 
#' the EA site (RBD, MC or OC downloadas) or a string of the WBID (for 
#' waterbody downloads).
#' 
#' @noRd

wbid_end_url <- function(data_type, index){
  switch(data_type,
         "class" = paste0("data/classification.csv?waterBody=", index, "&_view=csv"),
         "rnag" = paste0("data/reason-for-failure.csv?waterBody=", index, "&_view=csv"),
         "pa" = paste0("WaterBody/", index, "/pa/csv"),
         "objectives" = paste0("so/WaterBody/", index, "/objective-outcomes.csv?_view=csv"))
}

#' Set overall URL for downloads
#' @description Derive the overall download URL for all types (columns).
#' 
#' @param column A string representing the column type to be downloaded.
#
#' @param data_type A string representing the type of data (class, rnag, 
#' measures, objectives or pa) to be downloaded.
#' 
#' @param index Numeric index of RBD/OC/MC to be downloaded or for
#' WBID downloads the WBID code.
#' 
#' @noRd

set_url<-function(column, data_type, index){
  start_url<-"http://environment.data.gov.uk/catchment-planning/"
  switch(column,
       "RBD"=paste0(start_url, "RiverBasinDistrict/", index, set_end_url(data_type)),
       "MC"=paste0(start_url, "ManagementCatchment/", index, set_end_url(data_type)),
       "OC"=paste0(start_url, "OperationalCatchment/", index, set_end_url(data_type)),
        # wbids now consistent with others
        #"WBID"=paste0(start_url, wbid_end_url(data_type, index))
       "WBID"=paste0(start_url, "WaterBody/", index, set_end_url(data_type))
)
}

#' Find column index number
#' @description Find column index number (or WBID) for setting up 
#' download url.
#'  
#' @param column A string representing the column type to be downloaded.
#' 
#' @param ea_name A string representing the name of the catchment or 
#' WBID for individual waterbodies to be downloaded.
#' 
#' @noRd

find_index<-function(column, ea_name){
  switch(column,
         "RBD"=ea_wbids$RBD_num[which(ea_wbids[, column] == ea_name)][1],
          "MC"=ea_wbids$MC_num[which(ea_wbids[, column] == ea_name)][1],
          "OC"=ea_wbids$OC_num[which(ea_wbids[, column] == ea_name)][1],
          "WBID"=ea_wbids$WBID[which(ea_wbids[, column] == ea_name)][1]
)
}

#' Download Zipfile and extract csv
#' @description Downloads zipfile from specified url, unzips to
#' csv file and reads csv into dataframe.
#' 
#' @importFrom utils download.file
#' @importFrom utils unzip
#' @importFrom data.table fread
#
#' @param download_url A string representing the url to download the
#' zip file from.
#' 
#' @noRd

zip_download <- function(download_url) {
  temp <- tempfile()
  utils::download.file(download_url, temp, mode = "wb", quiet=FALSE)
  # extract data from zipfile to df using data.table to speed things up
  csvfile <- utils::unzip(temp, junkpaths = TRUE)
  message(paste0("Processing data from zipfile...","\n"))
  cde_data <- data.table::fread(csvfile, stringsAsFactors = FALSE, 
      check.names = TRUE, data.table = FALSE, showProgress = TRUE)
  # delete the intermediate files
  unlink(temp)
  unlink(csvfile)
  return(cde_data)
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

check_args <- function(ea_name = NULL, column = NULL, startyr = NULL, 
                        endyr = NULL, type = NULL) {
   # check that both ea_name and column are present
  if (is.null(ea_name) | is.null(column)) {
    stop("Both ea_name (name) and column (\"WBID\", \"MC\", \"OC\", 
         or \"RBD\") should be specified", "\n")
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
    types <- c("River", "Lake", "TransitionalWater", "GroundWaterBody", 
               "CoastalWater")
    if (!type %in% types) {
      stop("Type specified is not a valid choice (\"River\", \"Lake\", 
           \"CoastalWater\", \"TransitionalWater\" or \"GroundWaterBody\"")
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
#' @param column The column to be searched. Possible options are
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District)
#'
#' @param level The level within the WFD quality status classification to be
#' extracted. Defaults to 'Overall Water Body'. See \code{\link{get_status}}.
# for possible values.
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
#' @param data_type Type of data to be subset ("class", "obj" or "rnag"). If
#' "obj" then don't do subsetting to most recent as this is not appropriate.
#'
#' @return A data frame that has been subsetted by the 
#' specified combination of column, value, level and dates.
#'
#' @noRd
#'
subset_data <- function(full_data, column = NULL, 
    level = "Overall Water Body", startyr = NULL, endyr = NULL, type = NULL, data_type=NULL) {
  
  # if start year is set beyond the data range and not objectives
  # (only value being passed in to data_type) then set to max and ignore endyr
  if (!is.null(startyr) & is.null(data_type)){
    if (startyr>max(full_data$year)){
      message(paste0("Start year (", startyr, ") is beyond the most recent year of data (", 
                     max(full_data$year),")"))
      message("Just outputting most recent year")
      startyr<-max(full_data$year)
      # as only outputting most recent year, set endyr to null
      if(!is.null(endyr)){
        endyr<-NULL
      }
    }
  }
  # if endyr is set, is it beyond the data range?
  if (!is.null(endyr)){
    if (endyr>max(full_data$year)){
      message(paste0("End year (", endyr, ") is beyond the most recent year of data (", 
                     max(full_data$year),")"))
      message("Subsetting to most recent year")
      endyr<-max(full_data$year)
    }
  }
  if (!is.null(startyr)) {
    if(is.null(endyr)){
    # if only single year specified, just take this
      full_data <- full_data[full_data$year == startyr, ]
    }else{
      # if both years are specified, subset by range
      full_data <- full_data[full_data$year >= startyr 
                             & full_data$year <= endyr, ]
    }
  }
  # level subsetting, defaults to "Overall Water Body"
  # for Chemical and Supporting Elements levels, need to deal with options for
  # surface waters and groundwaters
  if (!is.null(level)){
    if (level == "Chemical") {
      full_data <- full_data[full_data$classification_item == "Chemical" | 
                        full_data$classification_item == "Chemical (GW)", ]
    }
    else if (level == "Supporting elements") {
      full_data <- full_data[full_data$classification_item == 
                      "Supporting elements (Surface Water)" | 
                        full_data$classification_item == 
                        "Supporting elements (Groundwater)", ]
    }
    else {
      full_data <- full_data[full_data$classification_item == level, ]
    }
  }
  # now water_body_type
  if (!is.null(type)) {
    full_data <- full_data[full_data$water_body_type == type, ]
  }
  
  # if year range covers 2013 and 2014, subset to just include cycle 2 data
  # avoids double counting of waterbodies
  if ("cycle" %in% names(full_data)) {
    full_data <- full_data[!(full_data$year == 2013 & full_data$cycle == 1 | 
                             full_data$year == 2014 & full_data$cycle == 1), ]
  }
  return(full_data)
} # end of function
