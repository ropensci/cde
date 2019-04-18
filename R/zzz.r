.onLoad <- function(libname = find.package("cde"), pkgname = "cde") {
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(
      c("ea_wbids")
    )
  invisible()
}