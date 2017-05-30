#' Download EA Catchment Data
#' @description Downloads classification data from EA Catchment Data
#' Explorer site. Data can be downloaded by specifying waterbody id 
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

#' @return A data frame containing the classifcation details for the 
#' specified combination of column and value.
#' 
#' @export download_ea
#'

download_ea<-function(col_value=NULL, column=NULL){
  # list of possible columns to select on
  choices<-c("WBID", "MC", "OC", "RBD")
  # is a value/column specified
  if (!is.null(column) & !is.null(col_value)){
    # is the column specified correctly
    if (column %in% choices){
      if (column =="RBD"){
      # rbd level extraction
        index_num<-ea_wbids$RBD.num[which(ea_wbids[,column]==col_value)][1]
        if (length(index_num)<1){
          message("River Basin District name specified not found.")
        }else{
          downloadurl<-paste0("http://environment.data.gov.uk/catchment-planning/RiverBasinDistrict/", index_num, "/classification?item=all&status=all&format=csv")
          classifications<-zip_download(downloadurl)
        }
      } # end of rbd extraction
      else if (column =="MC"){
        # rbd level extraction
        index_num<-ea_wbids$MC.num[which(ea_wbids[,column]==col_value)][1]
        if (length(index_num)<1){
          message("Management Catchment name specified not found.")
        }else{
          downloadurl<-paste0("http://environment.data.gov.uk/catchment-planning/ManagementCatchment/", index_num, "/classification?item=all&status=all&format=csv")
          classifications<-zip_download(downloadurl)
        }
      } # end of mc extraction
      # oc next
      else if (column =="OC"){
        # oc level extraction - works
        index_num<-ea_wbids$OC.num[which(ea_wbids[,column]==col_value)][1]
        if (length(index_num)<1){
          message("Operational catchment name specified not found.")
        }else{
          classifications<-utils::read.csv(paste0("http://environment.data.gov.uk/catchment-planning/OperationalCatchment/" , index_num, "/classification?item=all&status=all&format=csv"), header=TRUE, stringsAsFactors =  FALSE)
        }
      }# end of oc extraction
      # finally wbid
      else if (column =="WBID"){
        # oc level extraction
        if (col_value %in% ea_wbids[,"WBID"]){
          classifications<-utils::read.csv(paste0("http://environment.data.gov.uk/catchment-planning/WaterBody/", col_value, "/csv"), header=TRUE, stringsAsFactors =  FALSE)
        }
        else{
          message("WBID value specified not found.")
        }
      }# end of wbid extraction
    }else{
      stop("Column specified should be one of WBID, MC, OC or RBD")
    }
  }
} # end of function


# zip download and extraction function
zip_download<-function(downloadurl){
  temp<-tempfile()
  curl::curl_download(downloadurl, temp, mode="wb")
  # extract data from zipfile to df
  csv<-utils::unzip(temp, junkpaths=TRUE)
  catchment_data<-data.table::fread(csv, stringsAsFactors = FALSE, check.names=TRUE, data.table=FALSE)
  unlink(temp)
  unlink(csv)
  return(catchment_data)
# end of function
}