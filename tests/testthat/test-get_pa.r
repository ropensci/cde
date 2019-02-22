library(cde)
context("testing Protected Area downloads")

test_that("get_pa returns a dataframe", {
  # retrieve data for specific waterbody
  testframe<-get_pa("GB112071065700", "WBID")
  # check that it outputs a dataframe object
  expect_true(is.data.frame(testframe))
})

test_that("dimensions of dataframe are as expected", {
  # retrieve data
  test_wb<-get_pa("GB112071065700", "WBID")
  # check that the dimensions are 2 rows, 8 cols
  expect_true(all(dim(test_wb)== c(2, 8)))
})

test_that("invalid column specified returns an error", {
  # retrieve data for column "Aardvark"
  expect_error(get_pa("Avon Warwickshire", "Aardvark"))
})

test_that("incorrect arguments returns an error", {
  # left out column value to search on
  expect_error(get_pa("Avon Hampshire"))
})

test_that("invalid string returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(get_pa("Aardvark", "RBD"))
})
