library(cde)
context("testing objective downloads")

test_that("get_objectives returns a dataframe", {
  # retrieve data for specific waterbody
  testframe<-get_objectives("GB112071065700", "WBID")
  # check that it outputs a dataframe object
  expect_true(is.data.frame(testframe))
})

test_that("dimensions of dataframe are as expected", {
  # retrieve data for Avon Warwickshire MC
  test_mc_avon<-get_objectives("Avon Warwickshire", "MC", year=2015)
  # check that the dimensions are 20 rows, 20 cols
  expect_true(all(dim(test_mc_avon)== c(20, 20)))
})

test_that("invalid column specified returns an error", {
  # retrieve data for column "Aardvark"
  expect_error(get_objectives("Avon Warwickshire", "Aardvark"))
})

test_that("incorrect arguments returns an error", {
  # left out column value to search on
  expect_error(get_objectives("Avon Hampshire", year=2015))
})

test_that("incorrect year returns an error", {
  # left out column value to search on
  expect_error(get_objectives("Avon Hampshire", "MC", year=2013))
})

test_that("invalid string returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(get_objectives("Aardvark", "WBID"))
})

test_that("invalid type returns an error", {
  # retrieve data for type "Aardvark"
  expect_error(get_objectives("Avon Hampshire", "MC", year=2015, type="Aardvark"))
})

test_that("invalid level returns an error", {
  # retrieve data for level "Aardvark"
  expect_error(get_objectives("Avon Hampshire", "MC", level="Aardvark"))
})

test_that("specifying type for WB download returns a message", {
  # retrieve data for level that does not exist
  expect_message(get_objectives("GB520804714300", "WBID", type="River"))
})
