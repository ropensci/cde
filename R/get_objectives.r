#' Retrieve Objectives
#' @description Retrieves details of objectives set for waterbodies in terms
#' of predicted classification from EA Catchment Data Explorer site.
#' Data can be retrieved by specifying waterbody id
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
#' @param level The level within the WFD quality classification that 
#' objectives have been set at. Defaults to 'Overall Water Body'. 
#' See information in the Vignette for possible values.
#'
#' @param year The year that objectives are set for, either 2015, 
#' 2021 or 2027. If not given then objectives for all years are returned.
#' Note that objectives may not be set for all years.
#'
#' @param type Type of waterbody to be extracted. For Operational/Management
#' catchment level or RBD level queries, the data can also be subset by
#' waterbody type. Possible values are \code{River}, \code{Lake},
#' \code{GroundWaterBody}, \code{TransitionalWater} or \code{CoastalWater}.
#'
#' @return A data frame containing the details of the objectives set for
#' the specified set of waterbodies.
#'
#' @export get_objectives
#'
#' @examples
#' # get all objectives set for waterbody GB112071065700
#' \dontrun{get_objectives("GB112071065700", "WBID")}
#' 
#' # get the objectives set for Lakes in the Humber RBD, for the year 2021
#' \dontrun{get_objectives("Humber", "RBD", year=2021, type="Lake")}
#' 
#' # get the objectives set for Rivers in the Avon Warwickshire
#' # Operational Catchment in relation to Chemical status
#' \dontrun{get_objectives("Avon Warwickshire", "MC", level="Chemical", type = "River")}
#' 
get_objectives <- function(ea_name = NULL, column = NULL, 
  level="Overall Water Body", year = NULL, type = NULL) {
  # start by running general checks on input data
  check_args(ea_name, column, NULL, NULL, type)
  # list of possible columns to select on
  choices <- c("WBID", "MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices 
         (\"WBID\", \"OC\", \"MC\" or \"RBD\").")
  }
  
  # check year is one of options
  years <- c(2015, 2021, 2027)
  if (!is.null(year)){
    if (!year %in% years) {
      stop("Year specified is not one of the possible choices 
           (2015, 2021 or 2027).")
    }
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

  # if all inputs valid, download data
  obj_data <- download_cde(ea_name, column, data_type="objectives")

    # rename columns for consistency with get_status
  if (column=="WBID"){
    names(obj_data)[which(names(obj_data) == 
        "water.body.type")] <- "Water.body.type"
    names(obj_data)[which(names(obj_data) == 
        "River.Basin.District")] <- "River.basin.district"
    names(obj_data)[which(names(obj_data) == 
        "Management.Catchment")] <- "Management.catchment"
    names(obj_data)[which(names(obj_data) == 
        "Operational.Catchment")] <- "Operational.catchment"
  }
  if (column!="WBID"){
    names(obj_data)[which(names(obj_data) == 
        "River.Basin.District")] <- "River.basin.district"
    names(obj_data)[which(names(obj_data) == 
        "Management.Catchment")] <- "Management.catchment"
    names(obj_data)[which(names(obj_data) == 
        "Operational.Catchment")] <- "Operational.catchment"
  }
  
  # if there are no objectives set, give a message
  if (nrow(obj_data)==0){
    message("No objectives specified - empty dataframe returned")
    return(as.cde(obj_data))
  } else{
    # subset data by specified values
    # if WBID - don't subset by type and give message
    if (column=="WBID"){
      if (!is.null(type)){
        type<-NULL
        message("Type is ignored for WBID objectives")
      }
    }
    # subset data as required
    obj_data<-subset_data(obj_data, column, level, startyr=year, 
        endyr=NULL, type)
    # if there are no objectives returned, give a message
    if (nrow(obj_data)==0){
      message("No objectives specified - empty dataframe returned")
    }
    return(as.cde(obj_data))
  }
} # end of function
