#' Download EA Catchment Data
#' @description Downloads classification data from EA Catchment Data
#' Explorer site. Data can be downloaded by specifying waterbody id 
#' (\code{wbid}), Management Catchment (\code{mc}), Operational 
#' Catchment (\code{oc}) or River Basin District (\code{rbd}).
#' Start year (\code{startyr}) and end year (\code{endyr}) allow 
#' specific timeranges to be downloaded.
#' For Management Catchment (\code{mc}), Operational 
#' Catchment (\code{oc}) or River Basin District (\code{rbd}) level
#' downloads, waterbody type can also be specified.
#
#' @param col_value A string representing the features to be extracted. For
#' example to extract data for the whole of the Humber RBD, this would be 
#' "Humber". Must be an exact match to the values used in the EA database.
#' Use the \code{\link{search_sites}} function to check for matching values.
#' @param column The column to be searched. Possible options are 
#' \code{wbid} (waterbody id), \code{oc} (Operational Catchment), \code{mc} 
#' (Management Catchment) and \code{rbd} (River Basin District)
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

# EA data download - currently does some subsetting as well which needs to 
# be taken out to main function, but have to pass number in
# col_value and col_name should be in main really

# rbd (zip): http://environment.data.gov.uk/catchment-planning/RiverBasinDistrict/4/classification?item=all&status=all&format=csv
# mc (zip): http://environment.data.gov.uk/catchment-planning/ManagementCatchment/3001/classification?item=all&status=all&format=csv
# oc (csv): http://environment.data.gov.uk/catchment-planning/OperationalCatchment/3097/classification?item=all&status=all&format=csv
# wb (csv): http://environment.data.gov.uk/catchment-planning/WaterBody/GB30431693/csv


download_ea<-function(col_value=NULL, column=NULL, type=NULL, startyr=NULL, endyr=NULL){
  # list of possible columns to select on
  choices<-c("wbid", "mc", "oc", "rbd")
  # is a value/column specified
  if (!is.null(column) & !is.null(col_value)){
    # is the column specified correctly
    if (column %in% choices){
      if (column =="rbd"){
      # rbd level extraction
        index_num<-ea_wbids$number[which(ea_wbids[,column]==col_value)][1]
        if (length(index_num)<1){
          message("Column value specified not found")
        }else{
          downloadurl<-paste0("http://environment.data.gov.uk/catchment-planning/RiverBasinDistrict/", index_num, "/classification?item=all&status=all&format=csv")
          classifications<-zip_download(downloadurl)
        }
      } # end of rbd extraction
      else if (column =="mc"){
        # rbd level extraction
        index_num<-ea_wbids$mc.num[which(ea_wbids[,column]==col_value)][1]
        if (length(index_num)<1){
          message("Column value specified not found")
        }else{
          downloadurl<-paste0("http://environment.data.gov.uk/catchment-planning/ManagementCatchment/", index_num, "/classification?item=all&status=all&format=csv")
          classifications<-zip_download(downloadurl)
        }
      } # end of mc extraction
      # oc next
      else if (column =="oc"){
        # oc level extraction - works
        index_num<-ea_wbids$oc.num[which(ea_wbids[,column]==col_value)][1]
        print (index_num)
        if (length(index_num)<1){
          message("Column value specified not found")
        }else{
          classifications<-read.csv(paste0("http://environment.data.gov.uk/catchment-planning/OperationalCatchment/" , index_num, "/classification?item=all&status=all&format=csv"), header=TRUE, stringsAsFactors =  FALSE)
        }
      }# end of oc extraction
      # finally wbid - this works
      else if (column =="wbid"){
        # oc level extraction
        if (col_value %in% ea_wbids[,"wbid"]){
          classifications<-read.csv(paste0("http://environment.data.gov.uk/catchment-planning/WaterBody/", col_value, "/csv"), header=TRUE, stringsAsFactors =  FALSE)
        }
        else{
          message("Column value specified not found")
        }
      }# end of wbid extraction
    }else{
      cat("Column specified should be one of the following:", "\n")
      cat("wbid, mc, oc or rbd", "\n")
    }
  }
} # end of function


# zip download and extraction function
zip_download<-function(downloadurl){
  temp<-tempfile()
  curl_download(downloadurl, temp, mode="wb")
  # extract data from zipfile to df
  catchment_data<-fread(unzip(temp, junkpaths=TRUE), stringsAsFactors = FALSE, check.names=TRUE, data.table=FALSE)
  unlink(temp)
  return(catchment_data)
# end of function
}