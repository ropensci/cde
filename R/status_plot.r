#' Plot Status Summary
#' @description Produces a stacked proportional barplot of waterbody
#' status information for a given set of data (MC, OC or RBD)

#' @inheritParams wfd_class
#'
#' @return A stacked barplot of the percentage of waterbodies within the 
#' specified area of different status values, with standard colour scheme.
#'  
status_plot<-function(col_value=NULL, column=NULL, element="Overall Water Body", startyr=NULL, endyr=NULL, type=NULL){
  # do initial check of column choice
  plot_choices<-c("MC", "OC", "RBD")
    if (!column %in% plot_choices){
      stop("Column specified is not one of the possible choices (OC, MC or RBD).")
    }
  # get required data
  plot_data<-wfd_class(col_value, column, element, startyr, endyr, type)
  # convert to summary table
  plot_table<-with(plot_data, table(Status, Year))
  
  # convert to percentages
  props<-as.matrix(round(prop.table(plot_table, 2)*100),1)

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
  props<-props[ordered,]
  
  # do the actual plotting
  # need to scale x axis still - based on cols of props
  # return(barplot(props, legend=TRUE, col=cols_ordered, xlab="Year", ylab="Percentage of waterbodies", xlim=c(0.4, 10), ylim=c(0,100), args.legend=list(x=10.7,y=80)))
  
  return(barplot(props, legend=TRUE, col=cols_ordered, xlab="Year", ylab="Percentage of waterbodies", xlim=c(0.4, 10), ylim=c(0,100)))
  ##############
}# end of function