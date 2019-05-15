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
#' retrieved by the function are shown below.
#' \tabular{ccc}{
#' \strong{Level 1} \tab \strong{Level 2} \tab \strong{Level 4}\cr
#' Ecological \tab Biological quality elements \tab Overall Water Body\cr
#' Chemical \tab Chemical Status element \tab -\cr
#' Quantitative \tab Hydromorphological Supporting Elements \tab -\cr
#' - \tab Other Substances \tab -\cr
#' - \tab Physico-chemical quality elements \tab -\cr
#' - \tab Priority hazardous substances \tab -\cr
#' - \tab Priority substances \tab -\cr
#' - \tab Quantitative Status element \tab - \cr
#' - \tab Specific pollutants \tab -\cr
#' - \tab Supporting elements \tab -\cr
#' }
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
#' @return An object of class \code{cde_df} containing the classification 
#' details for the specified combination of criteria.
#' For details of the meaning of the the different columns returned, 
#' see \url{https://ropensci.github.io/cde/articles/cde-output-reference.html}.

#'
#' @export get_status
#'
#' @examples
#' # get Overall Water Body status classification for waterbody GB520804714300
#' get_status(ea_name="GB520804714300", column="WBID")
#' 
#' # get status class based on Priority substances for waterbody GB520804714300
#' get_status(ea_name="GB520804714300", column="WBID", level="Priority substances")
#' 
#' # get the Overall Water Body status of Lakes in the Humber RBD, between
#' # 2012 and 2014
#' get_status(ea_name="Humber", column="RBD", startyr=2012, endyr=2014, type="Lake")
#' 
#' # get the Overall Water Body status for Rivers in the Avon Warwickshire
#' # Operational Catchment in 2011
#' get_status(ea_name="Avon Warwickshire", column="MC", startyr=2011, type="River")
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
    message("Type is ignored for WBID downloads")
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
    status_data<-subset_data(status_data, column, level, startyr, endyr, type, data_type=NULL)
    if (nrow(status_data)==0){
      message("No status data for combination specified - 
        empty dataframe returned")
    }
  }
  # remove classification_id column - not needed
  status_data  <-  status_data[,!(names(status_data) %in% c("classification_id"))]
  # add comment for plot method identification
  comment(status_data) <- paste0("class;", column)
  return(as.cde(status_data))
} # end of function
