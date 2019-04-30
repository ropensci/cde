#' Class definition for cde_df
#' @description Output from a call to the EA website is returned as an
#' object of class \code{cde_df}, which is basically a dataframe with 
#' modified \code{print} method to format the output to fit the console 
#' width and a \code{plot} method to produce default plots of the data.
#
#' @param x A dataframe to be converted to class \code{cde_df}
#'
#'@noRd

as.cde <- function(x) {
  class(x) <- c("cde_df", setdiff(class(x), "cde_df"))
  x
}

#' Print method definition for cde_df
#' @description Custom \code{print} method for objects of class \code{cde_df}.
#' Formats output to fit current width of console.
#' 
#' @param x An object of class \code{cde_df}.
#' 
#' @param ... Other arguments passed on to individual methods. None 
#' implemented at present.
#'
#' @method print cde_df
#' @export

print.cde_df <- function(x, ...){
  # find number of columns that will fit on current console width
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
    data_to_print <- as.data.frame(t(apply(data_to_print, 1, trunc_char, 
      col_name_lengths)))
    print(data_to_print, row.names=FALSE)
  }else{cat("No data returned - printing not possible")}
  
  # if more than 10 rows, indicate missing data
  if(nrow(x)>10  & ncol(x)>ncol(data_to_print)){
    cat(paste0("With an additional ", nrow(x)-10, " rows and ", 
      ncol(x)-ncol(data_to_print), " columns of data."),"\n")
  }
  if (nrow(x)<11  & ncol(x)>ncol(data_to_print)){
    cat(paste0("With an additional ", ncol(x)-ncol(data_to_print), 
      " columns of data."),"\n")
  }
  if(nrow(x)>10 & ncol(x)<=ncol(data_to_print)){
    cat(paste0("With an additional ", nrow(x)-10, " rows of data."),"\n")
  }
  cat("Row values may be truncated to fit console. \n")
  # end of function
}

#' Truncate strings within \code{cde_df} objects to fit console
#' @description Truncates the length of strings within rows of \code{cde_df}
#' objects to the same length as the column name, ensuring that they
#' fit current width of console.
#' 
#' @param x An object of class \code{cde_df}.
#' 
#' @param col_name_lengths Vector containing lengths of column names.
#'
#'@noRd
trunc_char <- function(x, col_name_lengths){
  if (is.character(x)==TRUE){
    substr(x,1,col_name_lengths)
  }
}

#' Plot method for \code{cde_df} output
#' @description Default plots of the output main \code{get_} functions.
#' Details of the plots for different data are given below.
#'  
#'  For \code{status} and \code{objectives} produces a (stacked) 
#'  percentage barplot of waterbody observed or predicted (objective) 
#'  status information for a given set of data. 
#' 
#' For \code{rnag}, \code{measures} or \code{pa} produces a frequency 
#' histogram. Links to information on the EA website about the columns 
#' plotted for each data type are given below:
#' 
#' \itemize{
#'   \item \href{https://tinyurl.com/y5gn492m}{rnag} 
#'   (pressure_tier_3)
#'   \item \href{https://tinyurl.com/y5gn492m}{measures} 
#'   (measure_tier_1)
#'   \item \href{https://tinyurl.com/y5gn492m}{pa} 
#'   (protected_area_type)
#'}   
#' Plotting is only possible for MC, OC or RBD downloads.
#' 
#' @param x An object of class \code{cde_df} to be plotted.
#' 
#' @param ... Other arguments passed on to individual methods. The only other
#' argument implemented at present is \code{scheme}. For \code{status} and 
#' \code{objectives} this defines which colour scheme to use with plots.It 
#' defaults to a viridis-based scheme (\code{scheme="vir"}). Alternatively, the 
#' colours specified in the WFD document ca be used by specifying 
#' \code{scheme="wfd"}.
#'
#' @importFrom graphics barplot
#' 
#' @importFrom graphics par
#' 
#' @method plot cde_df
#' @export
#' 
plot.cde_df <- function(x, ...) {
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
  plot_choice(x, meta_data[[1]][1], ...)
} # end of function

#' Function to select the right plot type depending on data
#' @description Based on the \code{data_type}, determines the appropriate
#' plot.
#' @param x An object of class \code{cde_df} to be plotted
#' 
#' @param data_type String representing The type of data to be plotted.
#'
#' @param scheme Which colour scheme to use with plots (only used for 
#' \code{status} and \code{objectives}).
#'
#' @importFrom graphics barplot
#'
#' @return A plot of the data supplied. Format of plot depends on data.
#'
#' @noRd

plot_choice<-function(x, data_type, scheme="vir"){
  switch(data_type, 
         "class" = plot_status(x, data_type, scheme=scheme),
         "rnag" = plot_categories(x, data_type),
         "measures" = plot_categories(x, data_type),
         "pa" = plot_categories(x, data_type),
         "objectives" = plot_status(x, data_type, scheme=scheme))
}


#' Plot frequency histogram of data
#' @description Plots frequency histogram of different columns depending on 
#' the type of data for \code{rnag}, \code{measures}and \code{pa}.
#' 
#' @param x An object of class \code{cde_df} to be plotted
#' 
#' @param data_type String representing The type of data to be plotted.
#'
#' @return A frequency histogram of the data supplied.
#' 
#' @noRd
plot_categories<-function(x, data_type){
  switch(data_type,
         "pa"=plot_histogram(x$protected_area_type, data_type),
         "measures"=plot_histogram(x$measure_type, data_type),
         "rnag"=plot_histogram(x$pressure_tier_3, data_type))
}

#' Plot frequency histogram of data
#' @description Plots frequency histogram of different columns depending on 
#' the type of data for \code{rnag}, \code{measures}and \code{pa}.
#' 
#' @param column Specific column to be plotted (depends on \code{data_type}).
#' 
#' @param data_type String representing The type of data to be plotted.
#'
#' @return A frequency histogram of the data supplied.
#' 
#' @noRd
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

#' Plot Status Summary
#' @description Produces a (stacked) percentage barplot of waterbody
#' status information for a given set of data (MC, OC or RBD).

#' @param x An object of class \code{cde_df} to be plotted.
#' 
#' @param data_type String representing The type of data to be plotted, 
#' either "class" or "objectives".
#'
#' @param scheme Which colour scheme to use with plots; defaults to a viridis
#' based scheme (\code{"vir"} but can also choose to use the colours specified
#' in the WFD document by specifying as \code{"wfd"}.
#'
#' @importFrom graphics barplot
#'
#' @return A (stacked) barplot of the percentage of waterbodies within the
#' specified area of different status values represented as different colours
#' depending on the scheme specified.
#'
#' @noRd
#' 
plot_status <- function(x, data_type, scheme="vir") {
  
  # check that scheme is specified correctly
  scheme_choices<-c("vir", "wfd")
  if (!scheme %in% scheme_choices){
    stop("scheme should be either \"vir\" or \"wfd\".")
  }
  plot_table <- with(x, table(status, year))
  
  # convert to percentages
  props <- as.matrix(prop.table(plot_table, 2) * 100)
  
  # set up df of rows, status grades and colours # adapting for other elements
  # second 5 is fail for chemical and priority subs
  # 6 is Does not require assessment = Gray
  nums <- c(1, 2, 2, 3, 4, 5, 5, 6)
  status <- c("High", "Good", "Supports Good", "Moderate", "Poor", 
              "Bad", "Fail", "Does not require assessment")
  vir_colours <- c(viridisLite::viridis(7, direction=-1), "#BEBEBE")
  wfd_colours <- c("Blue", "Green", "Green", "Yellow", "Orange", "Red", 
                   "Red", "Gray")
  statusdf <- cbind.data.frame(nums, status, vir_colours, wfd_colours)
  
  # subset df based on status classes present in dataset
  needed <- statusdf[match(row.names(props), statusdf$status), ]
  
  # order the numbers required in decreasing order to set sequence
  ordered <- order(needed$nums, decreasing = TRUE)
  
  # order the colours needed in the same way, depending on scheme choice
  if (scheme == "wfd") {
    cols_ordered <- as.character(needed$wfd_colours[ordered])
  }
  else {
    cols_ordered <- as.character(needed$vir_colours[ordered])
  }
  
  # order the proportions in the same order
  ord_props <- props[ordered, ]
  
  # set x axis label depending on data type
  if (data_type=="class"){
    xlabel<-"Year of assessment"
  }
  if (data_type=="objectives"){
    xlabel<-"Year objective to be achieved"
  }
  
  # do the actual plotting
  # single year, single status class
  if (ncol(props) == 1 & nrow(props) == 1) {
    return(graphics::barplot(ord_props, names.arg = needed$status, 
      col = cols_ordered, space = 0, ylab = "Percentage of waterbodies", 
      xlab="Status class", ylim = c(0, 100), cex.names=0.8, cex.axis=0.8, 
      cex.lab=0.8))
  }
  # single year, more than one status class
  if (ncol(props) == 1 & nrow(props) > 1) {
    return(graphics::barplot(ord_props, col = cols_ordered, space = 0, 
      xlab="Status class", ylab = "Percentage of waterbodies", ylim = c(0, 100),
      cex.names=0.8, cex.axis=0.8, cex.lab=0.8))
  }
  # more than one year, one status class
  if (ncol(props) > 1 & nrow(props) == 1) {
    return(graphics::barplot(ord_props, legend.text = needed$status, 
      args.legend = list(x = (ncol(props) * 2) - (ncol(props) / 2.5), 
      y = 80, bg="white", cex=0.8), col = cols_ordered, 
      ylab = "Percentage of waterbodies", xlab=xlabel, 
      xlim = c(0, (ncol(props) * 2) - ncol(props) / 2), ylim = c(0, 100),
      cex.names=0.8, cex.axis=0.8, cex.lab=0.8))
  }
  # more than one year, more than one status class
  else {
    return(graphics::barplot(ord_props, legend = TRUE, 
      args.legend = list(x = (ncol(props) * 2) - (ncol(props) / 2.5), 
      y = 80, bg="white", cex=0.8), col = cols_ordered, 
      ylab = "Percentage of waterbodies", xlab=xlabel, 
      xlim = c(0, (ncol(props) * 2) - ncol(props) / 2), ylim = c(0, 100), 
      cex.names=0.8, cex.axis=0.8, cex.lab=0.8))
  }
} # end of function
