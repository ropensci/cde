library(cde)
context("testing RNAG issue downloads")

test_that("get_rnag returns a dataframe", {
  # retrieve data for specific waterbody
  testframe<-get_rnag("GB112071065700", "WBID")
  # check that it outputs a dataframe object
  expect_true(is.data.frame(testframe))
})

test_that("dimensions of dataframe are as expected", {
  # retrieve data for Avon Warwickshire MC
  test_mc_avon<-get_rnag("Avon Warwickshire", "MC", startyr=2014)
  # check that the dimensions are 221 rows, 26 cols
  expect_true(all(dim(test_mc_avon)== c(221, 26)))
})

test_that("invalid column specified returns an error", {
  # retrieve data for column "Aardvark"
  expect_error(get_rnag("Avon Warwickshire", "Aardvark"))
})

test_that("incorrect arguments returns an error", {
  # left out column value to search on
  expect_error(get_rnag("Avon Hampshire", startyr=2012))
})

test_that("invalid string returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(get_rnag("Aardvark", "WBID"))
})

test_that("invalid type returns an error", {
  # retrieve data for type "Aardvark"
  expect_error(get_rnag("Avon Hampshire", "MC", startyr=2012, type="Aardvark"))
})

test_that("start date outside data range returns an error", {
  # retrieve data for year 1900
  expect_error(get_rnag("Avon Hampshire", "MC", startyr=1900))
})

test_that("end date before start date returns an error", {
  # retrieve data for impossible year range
  expect_error(get_status("Avon Hampshire", "MC", startyr=2012, endyr=1900))
})

