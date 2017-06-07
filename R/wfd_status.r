#' Retrieve WFD Status Information
#' @description Retrieves WFD Status class data from EA Catchment Data
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
#' @param element The WFD quality element to be extracted. Defaults to 
#' 'Overall Water Body'. See docs for possible values.
#' 
#' @param startyr The data can be extracted for specific years using the 
#' \code{startyr} and \code{endyr} arguments. If only \code{startyr} is 
#' specified this extracts for a particular year. If no years are specified 
#' all years are returned.
#' 
#' @param endyr The data can be extracted for specific years using the 
#' \code{startyr} and \code{endyr} arguments. The \code{endyr} should
#' only be specified if \code{startyr} is also included, otherwise it
#' is ignored and all years are returned.
#' 
#' @param type Type of waterbody to be extracted. For Operational/Management 
#' catchment level or RBD level queries, the data can also be subset by 
#' waterbody type. Possible values are \code{River}, \code{Lake}, 
#' \code{GroundWaterBody}, \code{TransitionalWater} or \code{CoastalWater}.
#' 
#' @return A data frame containing the classifcation details for the 
#' specified combination of column, value, element and dates.
#' 
#' @export wfd_status
#'
#' @examples
#' # get Overall Water Body status for waterbody GB520804714300
#' wfd_status("GB520804714300", "WBID")
#' 
#' # get the Overall Water Body status of Lakes in the Humber RBD, between 
#' # 2012 and 2014
#' wfd_status("Humber", "RBD", startyr = 2012, endyr = 2014, type = "Lake")
#' 
#' # get the Overall Water Body status for Rivers in the Avon Warwickshire 
#' # Operational Catchment in 2011
#' wfd_status("Avon Warwickshire", "MC", startyr = 2011, type = "River")
#' 

wfd_status<-function(col_value=NULL, column=NULL, element="Overall Water Body", startyr=NULL, endyr=NULL, type=NULL){
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
        if (startyr<2009 | startyr >2015){
          stop("Starting year cannot be before 2009 or after 2015")
        }
        # if there is an end year
        if (!is.null(endyr)){
          # check values make sense
          if (!endyr >= startyr){
            stop("End year is before Start year: please correct.")
          }
          # years are in correct order
          if (!startyr >=2009 | !endyr <=2015){
            stop("Years specified outside range of data available (2009-2015).")
          }
        }
      }
      if (!is.null(type)){
        types<-c("River", "Lake", "TransitionalWater", "GroundWaterBody", "CoastalWater")
        if (!type %in% types){
          stop("Type specified is not a valid choice (River, Lake, CoastalWater, TransitionalWater or GroundWaterBody")
        }
      }
      # if all inputs valid, download data
      ##### does not currently check that string given is valid choice - run through search_sites
      status_data<-download_ea(col_value, column)
    }
    else{
      stop("Column specified is not one of the possible choices (WBID, OC, MC or RBD).")
      }
  }
 
  # do subsetting here - years first
  if (!is.null(startyr) & !is.null(endyr)){
    # if both years are specified, subset by range
    status_data<-status_data[status_data$Year>=startyr & status_data$Year <=endyr, ]
  }
  else if (!is.null(startyr)){
    status_data<-status_data[status_data$Year==startyr, ]
  }
  # element subsetting, defaults to "Overall Water Body"
  status_data<-status_data[status_data$Classification.Item==element, ]
  # now Water.body.type
  if (!is.null(type)){
    status_data<-status_data[status_data$Water.body.type==type, ]
  }
  # if year range covers 2013 and 2014, subset to just include cycle 2 data
  # avoids double counting of waterbodies
  status_data<-status_data[!(status_data$Year==2013 & status_data$Cycle==1 | status_data$Year==2014 & status_data$Cycle==1),]
  
  # remove web link column - not really needed
  status_data<-status_data[,!(names(status_data) %in% c("classification.ID"))]
  
  return(status_data)
} # end of function
