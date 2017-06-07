#' Plot Status Summary
#' @description Produces a (stacked) percentage barplot of waterbody
#' status information for a given set of data (MC, OC or RBD)

#' @inheritParams wfd_status
#' 
#' @importFrom graphics barplot
#'
#' @return A (stacked) barplot of the percentage of waterbodies within the 
#' specified area of different status values, with standard colour scheme.
#'  
#' @export status_plot
#'  
#' @examples 
#' \dontrun{
#' 
#' # plot the Overall Water Body status of Lakes in the Humber RBD, betweeen 
#' 2012 and 2014
#' status_plot("Humber", "RBD", startyr=2012, endyr = 2014, type="Lake")
#' 
#' # plot the Overall Water Body status for Rivers in the Avon Warwickshire 
#' # Operational Catchment in 2011
#' status_plot("Avon Warwickshire, "OC", startyr=2011, type="River")
#' 
#' }
#'  
status_plot<-function(col_value=NULL, column=NULL, element="Overall Water Body", startyr=NULL, endyr=NULL, type=NULL){
  # do initial check of column choice
  plot_choices<-c("MC", "OC", "RBD")
    if (!column %in% plot_choices){
      stop("Column specified is not one of the possible choices (OC, MC or RBD).")
    }
  # get required data
  plot_data<-wfd_status(col_value, column, element, startyr, endyr, type)
  if (nrow(plot_data)==0){
    stop("No data returned, plotting not possible.")
  }
  # convert to summary table
  plot_table<-with(plot_data, table(Status, Year))
  
  # convert to percentages
  # remove rounding
  # props<-as.matrix(round(prop.table(plot_table, 2)*100),2)
  
  props<-as.matrix(prop.table(plot_table, 2)*100)

  # set up df of rows, status grades and colours
  nums<-c(1,2,3,4,5)
  status<-c("High", "Good", "Moderate", "Poor", "Bad")
  colours<-c("Blue", "Green", "Yellow", "Orange", "Red")
  statusdf<-cbind.data.frame(nums, status, colours)
  
  # subset df based on status classes present in dataset
  needed<-statusdf[match(row.names(props), statusdf$status),]
  
  # order the numbers required in decreasing order to set sequence
  ordered<-order(needed$nums, decreasing=TRUE)
  
  # order the colours needed in the same way
  cols_ordered<-as.character(needed$colours[ordered])
  
  # order the proportions in the same order
  ord_props<-props[ordered,]
  
  # do the actual plotting
  # if for a single year
  if (ncol(props)==1){
    return(graphics::barplot(ord_props, col=cols_ordered, space=0, ylab="Percentage of waterbodies", ylim=c(0,100)))
  }
  else{
    return(graphics::barplot(ord_props, legend=TRUE, args.legend=list(x=(ncol(props)*2)-(ncol(props)/2.5), y=80), col=cols_ordered, ylab="Percentage of waterbodies", xlim=c(0, (ncol(props)*2)-ncol(props)/2), ylim=c(0,100)))
  }
} # end of function