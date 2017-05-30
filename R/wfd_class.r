#' Retrieve WFD Classification Information
#' @description Retrieves classification data from EA Catchment Data
#' Explorer site. Data can be retrieved by specifying waterbody id 
#' (\code{WBID}), Management Catchment (\code{MC}), Operational 
#' Catchment (\code{OC}) or River Basin District (\code{RBD}).
#' Start year (\code{startyr}) and end year (\code{endyr}) allow 
#' specific timeranges to be downloaded.
#' For Management Catchment (\code{MC}), Operational 
#' Catchment (\code{OC}) or River Basin District (\code{RBD}) level
#' downloads, waterbody \code{Type} can also be specified to allow
#' extraction of specific waterbody types (River, Lake etc).
#
#' @param col_value A string representing the description (name) of the 
#' features to be extracted. For example to extract data for the whole of 
#' the Humber RBD, this would be "Humber"; also see examples. Must be an 
#' exact match to the values used in the EA database.
#' Use the \code{\link{search_sites}} function to search for specific values.
#' @param column The column to be searched. Possible options are 
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC} 
#' (Management Catchment) and \code{RBD} (River Basin District)
#' @param type Type of waterbody to be extracted. For Operational/Management 
#' catchment level or RBD level queries, the data can also be subset by 
#' waterbody type. Possible values are \code{River}, \code{Lake}, 
#' \code{Groundwater}, \code{Transitional} or \code{Coastal}.
#' @param startyr The data can be extracted for specific years using the 
#' \code{startyr} and \code{endyr} arguments. If only \code{startyr} is 
#' specified this extracts for a particular year.
#' @param endyr The data can be extracted for specific years using the 
#' \code{startyr} and \code{endyr} arguments. The \code{endyr} should
#' only be specified if \code{startyr} is also included, otherwise it
#' is ignored.
#' @return A data frame containing the classifcation details for the 
#' specified combination of column and value.
#' 
#' @examples
#'
wfd_class<-function(col_value=NULL, column=NULL, element="Overall status", startyr=NULL, endyr=NULL, type=NULL){
  # start by running checks on input data
  # first search value and column choice
  # list of possible columns to select on
  choices<-c("WBID", "MC", "OC", "RBD")
  # is a value/column specified
  if (!is.null(column) & !is.null(col_value)){
    if (column %in% choices){
      # a valid column has been chosen, next test years
      # if there is a startyr set
      if (!is.null(startyr)){
        # if there is an end year
        if (!is.null(endyr)){
          # check values make sense
          if (!endyr >= startyr){
            stop("End year is before Start year: please correct.")
          }
          # years are in correct order
          if (!startyr >=2009 & endyr <=2015){
            stop("Years specified outside range of data available (2009-2015).")
          }
        }
      }
      # should be here
      class_data<-download_ea(col_value, column)
    }
    else{
      stop("Column specified is not one of the possible choices (WBID, OC, MC or RBD).")
      }
  }
 
  
# do subsetting here  
  
} # end of function
  
  ## is there a start year?
    # if so, is there an end year
      # if so, is the end year 