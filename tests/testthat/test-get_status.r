library(cde)
context("testing wfd status downloads")

test_that("get_status returns a dataframe", {
  # retrieve data for specific waterbody
  testframe<-get_status("GB520804714300", "WBID")
  # check that it outputs a dataframe object
  expect_true(is.data.frame(testframe))
})

test_that("dimensions of dataframe are as expected", {
  # retrieve data for Avon Warwickshire MC
  test_mc_avon<-get_status("Avon Warwickshire", "MC")
  # check that the dimensions are 594 rows, 19 cols
  expect_true(all(dim(test_mc_avon)== c(594, 19)))
})

test_that("invalid column specified returns an error", {
  # retrieve data for column "Aardvark"
  expect_error(get_status("Avon Warwickshire", "Aardvark"))
})

test_that("incorrect arguments returns an error", {
  # left out column value to search on
  expect_error(get_status("Avon Hampshire", startyr=2012))
})

test_that("invalid string for WBID returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(get_status("Aardvark", "WBID"))
})

test_that("invalid string for OC returns an error", {
  # retrieve data for site "Aardvark"
  expect_error(get_status("Aardvark", "OC"))
})

test_that("invalid type returns an error", {
  # retrieve data for type "Aardvark"
  expect_error(get_status("Avon Hampshire", "MC", startyr=2012, 
                          type="Aardvark"))
})

test_that("start date outside data range returns an error", {
  # retrieve data for year 1900
  expect_error(get_status("Avon Hampshire", "MC", startyr=1900))
})

test_that("end date only returns an error", {
  # retrieve data for year 1900
  expect_error(get_status("Avon Hampshire", "MC", endyr=2015))
})

test_that("end date before start date returns an error", {
  # retrieve data for impossible year range
  expect_error(get_status("Avon Hampshire", "MC", startyr=2012, endyr=1900))
})

test_that("end date outside available range returns a message", {
  # retrieve data for years outside possible range
  expect_message(get_status("Avon Hampshire", "MC", startyr=2012, endyr=2100))
})

test_that("start date outside available range returns a message", {
  # retrieve data for years outside possible range
  expect_message(get_status("Avon Hampshire", "MC", startyr=2100))
})

test_that("start and end date outside available range returns a message", {
  # retrieve data for years outside possible range
  expect_message(get_status("Avon Hampshire", "MC", startyr=2100))
})

test_that("non numeric dates returns an error", {
  # retrieve data for years outside possible range
  expect_error(get_status("Avon Hampshire", "MC", startyr="Aardvark", 
                          endyr="Aardvark"))
})

test_that("incorrect level returns a error", {
  # retrieve data for level that does not exist
  expect_error(get_status("Avon Hampshire", "MC", level="Aaardvark", 
                          startyr=2012))
})

test_that("specifying type for WB download returns a message", {
  # retrieve data for level that does not exist
  expect_message(get_status("GB520804714300", "WBID", type="River"))
})

test_that("incorrect set of criteria returns a message", {
  # retrieve data for level that does not exist
  expect_message(get_status("Avon Warwickshire", "MC", type="CoastalWater"))
})
