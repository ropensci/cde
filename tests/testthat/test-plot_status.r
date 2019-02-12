library(cde)
context("testing status plots")

test_that("invalid column specified returns an error", {
  # retrieve data for column "Aardvark"
  expect_error(plot_status("Avon Warwickshire", "Aardvark"))
})

test_that("invalid string returns an error", {
  # retrieve data for MC "Aardvark"
  expect_error(plot_status("Aardvark", "MC"))
})

test_that("invalid type returns an error", {
  # retrieve data for type "Aardvark"
  expect_error(plot_status("Avon Hampshire", "MC", startyr=2012, type="Aardvark"))
})

test_that("incorrect arguments returns an error", {
  # left out column value to search on
  expect_error(plot_status("Avon Hampshire", startyr=2012))
})

test_that("start date outside data range returns an error", {
  # retrieve data for year 1900
  expect_error(plot_status("Avon Hampshire", "MC", startyr=1900))
})

test_that("end date before start date returns an error", {
  # retrieve data for impossible year range
  expect_error(plot_status("Avon Hampshire", "MC", startyr=2012, endyr=1900))
})

#test_that("end date outside available range returns an error", {
  # retrieve data for years outside possible range
#  expect_error(plot_status("Avon Hampshire", "MC", startyr=2012, endyr=2018))
#})

test_that("function returns a vector of given length", {
  # plot for Avon Warwickshire MC chemical status
  test_plot<-plot_status("Avon Warwickshire", "MC", level="Chemical")
  # check that the vector has a length of 8
  expect_true(length(test_plot)== 8)
})
