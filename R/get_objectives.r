#' Retrieve Objectives set for waterbodies
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
#' @param ea_name A string representing the description (\code{name} for 
#' \code{OC}, \code{MC} or \code{RBD} level downloads or \code{WBID} for 
#' individual waterbodies) of the features to be extracted. For example 
#' to extract data for the whole of the Humber RBD, this would be "Humber"; 
#' also see examples. Must be an exact match to the values used in the EA 
#' database. Use the \code{\link{search_names}} function to search for 
#' specific values.
#'
#' @param column The column to be searched. Possible options are
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC}
#' (Management Catchment) and \code{RBD} (River Basin District)
#' 
#' @param level The level within the WFD quality classification elements that 
#' objectives have been set at. For full details of the hierarchy of elements 
#' within the classification used, see \url{https://environment.data.gov.uk/catchment-planning/help#help-classification-hierarchy}.
#' 
#' Defaults to 'Overall Water Body'. Possible values for the different levels 
#' retrived by the function are shown below.
#' \tabular{ccc}{
#'  \strong{Level 1} \tab \strong{Level 2} \tab \strong{Level 4}\cr
#'  Ecological \tab Biological quality elements \tab Overall Water Body\cr
#'Chemical \tab Chemical Status element \tab -\cr
#'  Quantitative \tab Hydromorphological Supporting Elements \tab -\cr
#'  - \tab Other Substances \tab -\cr
#'  - \tab Physico-chemical quality elements \tab -\cr
#'  - \tab Priority hazardous substances \tab -\cr
#'  - \tab Priority substances \tab -\cr
#'  - \tab Quantitative Status element \tab - \cr
#'  - \tab Specific pollutants \tab -\cr
#'  - \tab Supporting elements \tab -\cr
#' }
#'
#' @param year The year that objectives are set for, either 2015, 
#' 2021, 2027, 2040 or 2050. If not given then objectives for all years 
#' are returned. Note that objectives may not be set for all years.
#'
#' @param type Type of waterbody to be extracted. For Operational/Management
#' catchment level or RBD level queries, the data can also be subset by
#' waterbody type. Possible values are \code{River}, \code{Lake},
#' \code{GroundWaterBody}, \code{TransitionalWater} or \code{CoastalWater}.
#'
#' @return An object of class \code{cde_df} containing the details 
#' of the objectives set for the specified set of waterbodies.
#' For details of the meaning of the the different columns returned, 
#' see \url{https://robbriers.github.io/cde/articles/cde-output-reference.html}.

#'
#' @export get_objectives
#'
#' @examples
#' # get all objectives set for waterbody GB112071065700
#' get_objectives(ea_name="GB112071065700", column="WBID")
#' 
#' # get the objectives set for Lakes in the Humber RBD, for the year 2021
#' get_objectives(ea_name="Humber", column="RBD", year=2021, type="Lake")
#' 
#' # get the objectives set for Rivers in the Avon Warwickshire
#' # Operational Catchment in relation to Chemical status
#' get_objectives(ea_name="Avon Warwickshire", "column=MC", level="Chemical", type="River")
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
  years <- c(2015, 2021, 2027, 2040, 2050)
  if (!is.null(year)){
    if (!year %in% years) {
      stop("Year specified is not one of the possible choices 
           (2015, 2021, 2027, 2040 or 2050).")
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
    # delete uri columns
    obj_data <- obj_data[,!grepl("_uri",names(obj_data))]
    # subset data as required
    obj_data<-subset_data(obj_data, column, level, startyr=year, 
        endyr=NULL, type, data_type="obj")
    # if there are no objectives returned, give a message
    if (nrow(obj_data)==0){
      message("No objectives specified - empty dataframe returned")
    }
    # add comment for plot method identification
    comment(obj_data) <- paste0("objectives;", column)
    return(as.cde(obj_data))
  }
} # end of function
