library(cde)
context("status_plot")

test_that("invalid column specified returns an error", {
  # retrieve data for column "Aardvark"
  expect_error(status_plot("Avon Warwickshire", "Aardvark"))
})

#test_that("invalid string returns an error", {
#  # retrieve data for site "Aardvark"
#  expect_error(status_plot("Aardvark", "MC"))
#})

test_that("invalid type returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(status_plot("Avon Hampshire", "MC", startyr=2012, type="Aardvark"))
})


test_that("start date outside data range returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(status_plot("Avon Hampshire", "MC", startyr=1900))
})

test_that("end date before start date returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(status_plot("Avon Hampshire", "MC", startyr=2012, endyr=1900))
})

test_that("end date outside available range returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(status_plot("Avon Hampshire", "MC", startyr=2012, endyr=2018))
})
