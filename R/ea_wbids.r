#' Details of name and index of all sites/catchments.
#'
#' Dataframe used by `cde` to construct API calls
#'
#' @format A data frame with 5237 rows and 9 variables:
#' \describe{
#'   \item{WBID}{identifier for individual waterbodies}
#'   \item{name}{detailed name of the site/catchment}
#'   \item{type}{type of waterbody (River, Lake etc.)}
#'   \item{OC}{Operational catchment name}
#'   \item{OC_num}{Index number of the Operational Catchment}
#'   \item{MC}{Management catchment name}
#'   \item{MC_num}{Index number of the Management Catchment}
#'   \item{RBD}{River Basin District name}
#'   \item{RBD_num}{Index number of the River Basin District}
#'   ...
#' }
#' @source \url{https://environment.data.gov.uk/catchment-planning/}
"ea_wbids"