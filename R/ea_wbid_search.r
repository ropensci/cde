#' Search EA site database
#' @description Searches the listing of EA monitoring sites to find rows
#' that contain the string provided. Can search by name (\code{waterbody}), 
#' Management Catchment (\code{mc}), Operational Catchment (\code{oc}) 
#' or River Basin District (\code{rbd}).
#
#' @param string The search string to be matched. Will match whole or partial
#' strings in the column values.
#' @param column The column to be searched. Possible options are 
#' \code{waterbody}, \code{oc} (Operational Catchment), \code{mc} 
#' (Management Catchment) and \code{rbd} (River Basin District)
#' @return A data frame containing the details of all the sites that match
#' the search string (full or partial matches) in the column specified.
#' @examples
#' search_sites("Avon", "OC")
#' search_sites("Tweed", "RBD")

search_sites<-function(string=NULL, column=NULL){
  # if there is a value passed for both arguments
  if (!is.null(column) & !is.null(string)){
  # if the column is found in ea_wbids  
    if (tolower(column) %in% tolower(names(ea_wbids))){
      # extract list of rows that match search string
      matching_rows<-ea_wbids[grep(string, ea_wbids[,column]), ]
      # remove indexing columns
      matching_rows<-matching_rows[, c(-5, -7, -9)]
    }else{
      cat("Column specified should be one of the following:", "\n")
      cat("name, MC, OC, or RBD", "\n")
    }
  }
  return(matching_rows)
  # should just return info on column searched on???
# end of function  
}