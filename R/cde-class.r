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
#' @param ... Other arguments passed on to individual methods
#'
#' @method print cde_df
#' @export

print.cde_df <- function(x, ...){
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
  if(nrow(x)>10  & ncol(x)>ncol(data_to_print)){
    cat(paste0("With an additional ", nrow(x)-10, " rows and ", ncol(x)-ncol(data_to_print), " columns of data."),"\n")
  }
  if (nrow(x)<11  & ncol(x)>ncol(data_to_print)){
    cat(paste0("With an additional ", ncol(x)-ncol(data_to_print), " columns of data."),"\n")
  }
  if(nrow(x)>10 & ncol(x)<=ncol(data_to_print)){
    cat(paste0("With an additional ", nrow(x)-10, " rows of data."),"\n")
  }
  cat("Row values may be truncated to fit console. \n")
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

#' Plot method for cde_df output
#' @description Default plots of the output of calls to `get...` functions.
#' For `status` and `objectives` produces a (stacked) percentage barplot of 
#' waterbody observed or predicted (objective) status information for a 
#' given set of data. 
#' 
#' For `rnag`, `measures` or `pa` produces a frequency histogram of the 
#' columns `pressure_tier_3` for `rnag`, `measures_tier_1` for `measures`
#'  and `protected_area_type` for `pa`. Details of the classifications used
#'  are given XXXXXX
#'
#' Plotting is only possible for MC, OC or RBD downloads.
#' 
#' @param x An object of class `cde_df` to be plotted
#' 
#' @param scheme Which colour scheme to use with plots; defaults to a viridis
#' based scheme (\code{"vir"}) but for \code{rnag} and \code{objectives} data 
#' the colours specified in the WFD document by specifying as \code{"wfd"}.
#' 
#' @param ... Other arguments passed on to individual methods
#'
#' @importFrom graphics barplot
#' 
#' @importFrom graphics par
#' 
#' @method plot cde_df
#' @export
#' 
plot.cde_df <- function(x, scheme = "vir", ...) {
  # set up the choice of plotting levels
  plot_choices <- c("MC", "OC", "RBD")

  # extract the data type from the comment string
  meta_data<-strsplit(attr(x, "comment"), ";")
  
  # if there are no rows, plotting not possible
  if (nrow(x)==0){
    stop("No data - plotting not possible")
  }
  
  # check if WBID - no plotting possible
  if (meta_data[[1]][2]=="WBID"){
    stop("Plot method is not defined for WBID level downloads.")
  }
  # if not defined or not in choices
  if(is.na(meta_data[[1]][2])| !meta_data[[1]][2] %in% plot_choices){
    stop("Type of data cannot be determined for plotting")
  }
  plot_choice(x, meta_data[[1]][1], scheme)
} # end of function

# document here
plot_choice<-function(x, data_type, scheme="vir"){
  switch(data_type, 
         "class" = plot_status(x, data_type, scheme=scheme),
         "rnag" = plot_categories(x, data_type),
         "measures" = plot_categories(x, data_type),
         "pa" = plot_categories(x, data_type),
         "objectives" = plot_status(x, data_type, scheme=scheme))
}


# control function to set column 
plot_categories<-function(x, data_type){
  switch(data_type,
         "pa"=plot_histogram(x$protected_area_type, data_type),
         # or measure_category_1
         "measures"=plot_histogram(x$measure_type, data_type),
         "rnag"=plot_histogram(x$pressure_tier_3, data_type))
}

# actual plotting function
plot_histogram<-function(column, data_type){
  # save the original graphics pars
  old.par <- par(no.readonly = TRUE)
  # change margins to fit column text lengths
  par(mar=c(5,(max(nchar(column))/2)-2,2,2))
  # do the actual plotting
  if (data_type=="rnag"){
    xlabel<-"Frequency of RNAG across all waterbodies"
  }
  if (data_type=="pa"){
    xlabel="Number of protected areas"
  }
  barplot(sort(table(column), decreasing=TRUE), horiz=TRUE, cex.names=0.8,
          cex.axis=0.8, las=2,space=0,col=viridisLite::viridis(nrow(table(column))), 
          xpd=FALSE, xlab=xlabel, cex.lab=0.8)
  # reset the graphics pars
  par(old.par)
}


# rnag - category, swmi or pressure_tier_1
# objectives - predicted and year [status crosstab]
# measures - measure_category_1 or measure_type
# pa - protected_area_type - numbers rather than %??