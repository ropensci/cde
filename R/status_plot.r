#' Plot Status Summary
#' @description Produces a (stacked) percentage barplot of waterbody
#' status information for a given set of data (MC, OC or RBD)

#' @inheritParams wfd_status
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
#' @export status_plot
#'  
#' @examples
#' # plot the Overall Water Body status for Rivers in the Avon Warwickshire 
#' # Management Catchment in 2011 using viridis-based colours
#' status_plot("Avon Warwickshire", "MC", startyr = 2011, type = "River")
#' 
#' # plot the Overall Water Body status of Lakes in the Humber RBD betweeen 
#' # 2012 and 2014 using WFD colour scheme
#' status_plot("Humber", "RBD", startyr = 2012, endyr = 2014, type="Lake", scheme="wfd")
#'
status_plot<-function(col_value=NULL, column=NULL, element="Overall Water Body", startyr=NULL, endyr=NULL, type=NULL, scheme="vir"){
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
  vir_colours<-c("#79d051ff", "#26a784ff","#2a768eff", "#404284ff", "#440154ff")
  wfd_colours<-c("Blue", "Green", "Yellow", "Orange", "Red")
  statusdf<-cbind.data.frame(nums, status, vir_colours, wfd_colours)
  
  # subset df based on status classes present in dataset
  needed<-statusdf[match(row.names(props), statusdf$status),]
  
  # order the numbers required in decreasing order to set sequence
  ordered<-order(needed$nums, decreasing=TRUE)
  
  # order the colours needed in the same way, depending on scheme choice
  if (scheme=="wfd"){
    cols_ordered<-as.character(needed$wfd_colours[ordered])
  }
  else{
    cols_ordered<-as.character(needed$vir_colours[ordered])
  }
  
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