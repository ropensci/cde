library(cde)
context("testing RNAG issue downloads")

test_that("get_rnag returns a dataframe (as cde_df class)", {
  # retrieve data for specific waterbody
  testframe<-get_rnag("GB112071065700", "WBID")
  # check that it outputs a dataframe object
  expect_true(is.data.frame(testframe))
})

test_that("invalid column specified returns an error", {
  # retrieve data for column "Aardvark"
  expect_error(get_rnag("Avon Warwickshire", "Aardvark"))
})

test_that("incorrect arguments returns an error", {
  # left out column value to search on
  expect_error(get_rnag("Avon Hampshire"))
})

test_that("invalid string returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(get_rnag("Aardvark", "WBID"))
})

test_that("invalid type returns an error", {
  # retrieve data for type "Aardvark"
  expect_error(get_rnag("Avon Hampshire", "MC", ype="Aardvark"))
})

test_that("incorrect level returns a error", {
  # retrieve data for level that does not exist
  expect_error(get_rnag("Avon Hampshire", "MC", level="Aaardvark"))
})

test_that("specifying type for WB download returns a message", {
  # retrieve data for level that does not exist
  expect_message(get_rnag("GB112071065700", "WBID", type="River"))
})

test_that("downloading empty dataframe (no RNAG) returns a message", {
  # retrieve data for OC without RNAG
  expect_message(get_rnag("Cornwall North Coastal", "OC"))
})
