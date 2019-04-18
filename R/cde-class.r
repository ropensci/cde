
# define cde_df class (modified df)
as.cde <- function(x) {
  class(x) <- c("cde_df", setdiff(class(x), "cde_df"))
  x
}

# custom print method
print.cde_df <- function(x){
  # find number of columns that will fit on current width
  # if the maximum length of all column names is greater than the width
  # sebset the columns
  if(max(cumsum(nchar(names(x))+2)>getOption("width"))){
    cols<-min(which(cumsum(nchar(names(x))+2) > getOption("width")))-1
  }else{cols<-ncol(x)}
  # subset df for just these columns
  data_to_print <- x[,1:cols]
  
  # get column name lengths for use in truncation
  col_name_lengths<-nchar(names(data_to_print))
  
  # if there are more than 10 rows, just take first 10
  if (nrow(data_to_print)>10){
    data_to_print <- data_to_print[1:10,]
  }
  # truncate strings within rows to fit as well
  if(!nrow(x)==0){
    data_to_print <- as.data.frame(t(apply(data_to_print, 1, trunc_char, cols, col_name_lengths)))
    print(data_to_print, row.names=FALSE)
  }else{cat("No data returned - printing not possible")}
  # output data that fits
  
  # if more than 10 rows, indicate missing data
  if(nrow(x)>10){
    # #### need to handle if 11 or just one more column
    cat(paste0("With an additional ", nrow(x)-10, " rows and ", ncol(x)-ncol(data_to_print), " columns of data."),"\n")
    cat("Row values may be truncated to fit console.")
  }
    # end of function
}

# string truncation function
trunc_char <- function(x, cols, col_name_lengths){
  if (is.character(x)==TRUE){
    substr(x,1,col_name_lengths)
  }
}
