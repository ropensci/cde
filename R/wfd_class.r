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
#' 
#' @param column The column to be searched. Possible options are 
#' \code{WBID} (waterbody id), \code{OC} (Operational Catchment), \code{MC} 
#' (Management Catchment) and \code{RBD} (River Basin District)
#' @param element The WFD quality element to be extracted. Defaults to 'Overall
#' status'. See Vignette for possible values.
#' 
#' @param startyr The data can be extracted for specific years using the 
#' \code{startyr} and \code{endyr} arguments. If only \code{startyr} is 
#' specified this extracts for a particular year. If no years are specified 
#' all years are returned.
#' 
#' @param endyr The data can be extracted for specific years using the 
#' \code{startyr} and \code{endyr} arguments. The \code{endyr} should
#' only be specified if \code{startyr} is also included, otherwise it
#' is ignored.
#' 
#' @param type Type of waterbody to be extracted. For Operational/Management 
#' catchment level or RBD level queries, the data can also be subset by 
#' waterbody type. Possible values are \code{River}, \code{Lake}, 
#' \code{Groundwater}, \code{Transitional} or \code{Coastal}.
#' 
#' @return A data frame containing the classifcation details for the 
#' specified combination of column, value, element and dates.
#' 
#' @export wfd_class
#'
wfd_class<-function(col_value=NULL, column=NULL, element="Overall Water Body", startyr=NULL, endyr=NULL, type=NULL){
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
 
  # do subsetting here - years first
  if (!is.null(startyr) & !is.null(endyr)){
    # if both years are specified, subset by range
    class_data<-class_data[class_data$Year>=startyr & class_data$Year <=endyr, ]
  }
  else if (!is.null(startyr)){
    class_data<-class_data[class_data$Year==startyr, ]
  }
  # element subsetting, defaults to "Overall Water Body"
  class_data<-class_data[class_data$Classification.Item==element, ]
  # now Water.body.type
  if (!is.null(type)){
    class_data<-class_data[class_data$Water.body.type==type, ]
  }
  return(class_data)
} # end of function
