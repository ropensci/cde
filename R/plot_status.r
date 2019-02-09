#' Plot Status Summary
#' @description Produces a (stacked) percentage barplot of waterbody
#' status information for a given set of data (MC, OC or RBD)

#' @inheritParams get_status
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
#' @export plot_status
#'
#' @examples
#' # plot the Overall Water Body status for Rivers in the Avon Warwickshire
#' # Management Catchment in 2011 using viridis-based colours
#' plot_status("Avon Warwickshire", "MC", startyr = 2011, type = "River")
#' 
#' # Chemical status classification for same MC, but all years and types
#' plot_status("Avon Warwickshire", "MC", level = "Chemical")
#' 
#' # plot the Overall Water Body status of all waterbodies
#' # in the Humber RBD between 2012 and 2014 using WFD colour scheme
#' plot_status("Humber", "RBD", startyr = 2012, endyr = 2014, scheme = "wfd")
plot_status <- function(col_value = NULL, column = NULL, level = "Overall Water Body", startyr = NULL, endyr = NULL, type = NULL, scheme = "vir") {
  # are both col_value and column given?
  if (is.null(col_value) | is.null(column)) {
    stop("Both col_value (site name) and column (name, MC, OC, or RBD) should be specified", "\n")
  }
  # do initial check of column choice
  plot_choices <- c("MC", "OC", "RBD")
  if (!column %in% plot_choices) {
    stop("Column specified is not one of the possible choices (OC, MC or RBD).")
  }
  # check that scheme is specified correctly
  scheme_choices<-c("vir", "wfd")
  if (!scheme %in% scheme_choices){
    stop("scheme should be either \"vir\" or \"wfd\".")
  }
  # get required data
  plot_data <- get_status(col_value, column, level, startyr, endyr, type)
  if (nrow(plot_data) == 0) {
    stop("No data returned, plotting not possible.")
  }
  # convert to summary table
  plot_table <- with(plot_data, table(Status, Year))

  # convert to percentages
  props <- as.matrix(prop.table(plot_table, 2) * 100)

  # set up df of rows, status grades and colours # adapting for other elements
  # second 5 is fail for chemical and priority subs
  # 6 is Does not require assessment = Gray
  nums <- c(1, 2, 2, 3, 4, 5, 5, 6)
  status <- c("High", "Good", "Supports Good", "Moderate", "Poor", "Bad", "Fail", "Does not require assessment")
  vir_colours <- c("#79d051ff", "#26a784ff", "#26a784ff", "#2a768eff", "#404284ff", "#440154ff", "#440154ff", "#BEBEBE")
  wfd_colours <- c("Blue", "Green", "Green", "Yellow", "Orange", "Red", "Red", "Gray")
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

  # do the actual plotting
  # single year, single status class
  if (ncol(props) == 1 & nrow(props) == 1) {
    return(graphics::barplot(ord_props, names.arg = needed$status, col = cols_ordered, space = 0, ylab = "Percentage of waterbodies", ylim = c(0, 100)))
  }
  # single year, more than one status class
  if (ncol(props) == 1 & nrow(props) > 1) {
    return(graphics::barplot(ord_props, col = cols_ordered, space = 0, ylab = "Percentage of waterbodies", ylim = c(0, 100)))
  }
  # more than one year, one status class
  if (ncol(props) > 1 & nrow(props) == 1) {
    graphics::barplot(ord_props, legend.text = needed$status, args.legend = list(x = (ncol(props) * 2) - (ncol(props) / 2.5), y = 80, bg="white"), col = cols_ordered, ylab = "Percentage of waterbodies", xlim = c(0, (ncol(props) * 2) - ncol(props) / 2), ylim = c(0, 100))
  }
  # more than one year, more than one status class
  else {
    return(graphics::barplot(ord_props, legend = TRUE, args.legend = list(x = (ncol(props) * 2) - (ncol(props) / 2.5), y = 80, bg="white"), col = cols_ordered, ylab = "Percentage of waterbodies", xlim = c(0, (ncol(props) * 2) - ncol(props) / 2), ylim = c(0, 100)))
  }
} # end of function
