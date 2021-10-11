#' Retrieve Reasons for Not Achieving Good Status 
#' @description Retrieves details of Reasons for Not Achieving Good (RNAG)
#' status and Reasons For Failure (RFF) from EA Catchment Data Explorer site.
#' Data can be retrieved by specifying waterbody id
#' (\code{WBID}), Management Catchment (\code{MC}), Operational
#' Catchment (\code{OC}) or River Basin District (\code{RBD}).
#' Start year (\code{startyr}) and end year (\code{endyr}) allow
#' specific timeranges to be downloaded.
#' For Management Catchment (\code{MC}), Operational
#' Catchment (\code{OC}) or River Basin District (\code{RBD}) level
#' downloads, waterbody \code{type} can also be specified to allow
#' extraction of specific waterbody types (River, Lake etc).
#' Data are presented at the level of individual elements that are the
#' reasons for not achieving good status.
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
#' @param type Type of waterbody to be extracted. For Operational/Management
#' catchment level or RBD level queries, the data can also be subset by
#' waterbody type. Possible values are \code{River}, \code{Lake},
#' \code{GroundWaterBody}, \code{TransitionalWater} or \code{CoastalWater}.
#'
#' @return An object of class \code{cde_df} containing the details of the 
#' Reasons for Not Achieving Good Status for the specified combination 
#' of criteria.
#' For details of the meaning of the the different columns returned, 
#' see \url{https://docs.ropensci.org/cde/articles/cde-output-reference.html}.

#'
#' @export get_rnag
#'
#' @examples
#' # get all RNAG issues identified for waterbody GB112071065700
#' get_rnag("GB112071065700", "WBID")
#' 
#' # get the RNAG issues for Lakes in the Humber RBD, between
#' # 2013 and 2014
#' get_rnag(ea_name="Humber", column="RBD", type="Lake")
#' 
#' # get the RNAG issues for Rivers in the Avon Warwickshire
#' # Management Catchment
#' get_rnag(ea_name="Avon Warwickshire", column="MC", type="River")
#' 
get_rnag <- function(ea_name = NULL, column = NULL, type = NULL) {

  # start by running general checks on input data
  check_args(ea_name, column, type)
  # list of possible columns to select on
  choices <- c("WBID", "MC", "OC", "RBD")
  # check column is one of options
  if (!column %in% choices) {
    stop("Column specified is not one of the possible choices 
         (\"WBID\", \"OC\", \"MC\" or \"RBD\").")
  }
  # if all inputs valid, download data
  rnag_data <- download_cde(ea_name, column, data_type="rnag")

  # replace columns titles for consistency with other outputs
  names(rnag_data)[names(rnag_data) == "classification_status"] <- "status"
  names(rnag_data)[names(rnag_data) == "water_body_id"] <- "waterbody_id"
  
    # check if any data returned
  if (nrow(rnag_data)==0){
    message("No RNAG data - empty dataframe returned")
    return(as.cde(rnag_data))
  }else{
    # if WBID - don't subset by type and give message
    if (column=="WBID"){
      if (!is.null(type)){
        type<-NULL
        message("Type is ignored for WBID objectives")
      }
    }
    # subset data
    rnag_data<-subset_data(rnag_data, column, NULL, NULL, NULL, type, data_type="rnag")
    if (nrow(rnag_data)==0){
      message("No RNAG data - empty dataframe returned")
    }
    # add comment for plot method identification
    comment(rnag_data) <- paste0("rnag;", column)
    return(as.cde(rnag_data))
  }
  
} # end of function
