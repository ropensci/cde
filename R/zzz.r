#' Function to load ea_wbids data file with package
#' 
#' @importFrom utils globalVariables
#' 
#' @noRd
.onLoad <- function(libname = find.package("cde"), pkgname = "cde") {
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(
      c("ea_wbids")
    )
  invisible()
}