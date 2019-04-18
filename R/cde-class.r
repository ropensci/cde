#' Class definition for cde_df
#' @description Output from a call to the EA website is returned as an
#' object of class '`cde_df`, which is basically a dataframe with a 
#' modified `print` method to format the output ro fit the console width
#
#' @param x A dataframe to be converted to class `cde_df`
#'
#'@noRd

as.cde <- function(x) {
  class(x) <- c("cde_df", setdiff(class(x), "cde_df"))
  x
}

#' Print method definition for cde_df
#' @description Custom `print` method for objects of class '`cde_df`.
#' Formats output to fit current width of console.
#' 
#' @param x An object of class `cde_df`
#'
#'@noRd
print.cde_df <- function(x){
  # find number of columns that will fit on current width
  # if the maximum length of all column names is greater than the width
  # subset the columns
  if(max(cumsum(nchar(names(x))+2)>getOption("width"))){
    cols<-min(which(cumsum(nchar(names(x))+2) > getOption("width")))-1
  }else{cols<-ncol(x)}
  
  # subset cde_df for just these columns
  data_to_print <- x[,1:cols]
  
  # get column name lengths for use in truncation
  col_name_lengths<-nchar(names(data_to_print))
  
  # if there are more than 10 rows, just take first 10
  if (nrow(data_to_print)>10){
    data_to_print <- data_to_print[1:10,]
  }
  
  # truncate strings within rows to fit as well
  if(!nrow(x)==0){
    data_to_print <- as.data.frame(t(apply(data_to_print, 1, trunc_char, col_name_lengths)))
    print(data_to_print, row.names=FALSE)
  }else{cat("No data returned - printing not possible")}
  
  # if more than 10 rows, indicate missing data
  if(nrow(x)>10){
    # if there are more columns that visible on the screen
    if (ncol(x)>ncol(data_to_print)){
      cat(paste0("With an additional ", nrow(x)-10, " rows and ", ncol(x)-ncol(data_to_print), " columns of data."),"\n")
    }else{
      cat(paste0("With an additional ", nrow(x)-10, " rows of data."),"\n")
    }
  }
  cat("Row values may be truncated to fit console.")
  # end of function
}

#' Truncate strings within `cde_df` objects to fit console
#' @description Truncates the length of strings within rows of `cde_df`
#' objects to the same length as the column name, ensuring that they
#' fit current width of console.
#' 
#' @param x An object of class `cde_df`
#' 
#' @param col_name_lengths Vector containing lengths of column names
#'
#'@noRd
trunc_char <- function(x, col_name_lengths){
  if (is.character(x)==TRUE){
    substr(x,1,col_name_lengths)
  }
}
