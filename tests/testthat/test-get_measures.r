library(cde)
context("testing measures downloads")

test_that("get_measures returns a dataframe", {
  # retrieve data for specific waterbody
  testframe<-get_measures("Thames", "RBD")
  # check that it outputs a dataframe object
  expect_true(is.data.frame(testframe))
})

test_that("invalid column specified returns an error", {
  # retrieve data for column "Aardvark"
  expect_error(get_measures("Avon Warwickshire", "Aardvark"))
})

test_that("incorrect arguments returns an error", {
  # left out column value to search on
  expect_error(get_measures("Avon Hampshire"))
})

test_that("invalid string returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(get_measures("Aardvark", "RBD"))
})
