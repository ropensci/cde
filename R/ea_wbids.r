#' Details of name and index of all sites/catchments.
#'
#' Dataframe used by `cde` to construct API calls.
#' The data included are made available under the Open Government Licence v3.0 
#' \url{https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/}.
#' Use of the data accessed by and contained within this package implies 
#' acceptance of these licence conditions.
#' 
#' @docType data
#'
#' @format A data frame with 5237 rows and 9 variables:
#' \describe{
#'   \item{WBID}{identifier for individual waterbodies}
#'   \item{name}{detailed name of the site/catchment}
#'   \item{type}{type of waterbody (River, Lake etc.)}
#'   \item{OC}{Operational Catchment name}
#'   \item{OC_num}{Index number of the Operational Catchment}
#'   \item{MC}{Management Catchment name}
#'   \item{MC_num}{Index number of the Management Catchment}
#'   \item{RBD}{River Basin District name}
#'   \item{RBD_num}{Index number of the River Basin District}
#' }
#' For details of the hierarchy of the different catchment types, see
#' \url{https://environment.data.gov.uk/catchment-planning/help#help-catchment-hierarchy} 
#' @source \url{https://environment.data.gov.uk/catchment-planning/}
"ea_wbids"
