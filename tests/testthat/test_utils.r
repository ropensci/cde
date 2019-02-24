library(cde)
context("utility function testing")

test_that("download returns a dataframe", {
  # retrieve data for specific waterbody
  testframe<-download_cde("GB520804714300", "WBID", "class")
  # check that it outputs a dataframe object
  expect_true(is.data.frame(testframe))
})


test_that("zip_download returns a dataframe", {
  # set up download URL for RBD level download
  test_url<-paste0("http://environment.data.gov.uk/catchment-planning/RiverBasinDistrict/6/classification?item=all&status=all&format=csv")
  testframe<-zip_download(test_url)
  # check that it outputs a dataframe object
  expect_true(is.data.frame(testframe))
})
