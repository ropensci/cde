# function to query wbid datafile that matches a string in waterbody
# description - output a df of possible sites.

# search options:
# by wbid, RBD (whole file download), Catchment (Management), waterbody
# classification level, year range, 

# reverse column and string for vectorisation as per main function
search_sites<-function(column=NULL, string=NULL){
  # copy df- not really needed I think
  seachdf<-ea_wbids
  # convert to character - need to do this to datafile object loaded
  searchdf[]<-lapply(ea_wbids, as.character)
  # if there is a value passed for both arguments
  if (!is.null(column) & !is.null(string)){
  # if the column is found in ea_wbids  
    if (tolower(column) %in% tolower(names(ea_wbids))){
      # set up dummy strings for testing
#      string<-"Avon"
#      column<-"water.body"
      # extract list of rows that match search string
      matching_rows<-searchdf[grep(string, searchdf[,column]), ]
    }else{
      cat("Column specified should be one of the columns of the site listing.", "\n")
      cat(colnames(ea_wbids), "\n")
#      message(colnames(ea_wbids))
    }
  }
# end of function  
}