library(cde)
context("search for sites by name")

test_that("dimensions of search_name dataframe are as expected", {
  # search for sites containing "Avon" in their name
  test_search_avon<-search_names("Avon", "name")
  # check that the dimensions are 103 rows, 6 cols
  expect_true(all(dim(test_search_avon)== c(103, 6)))
  
})

test_that("invalid site name returns an error", {
  # search for sites containing "Aardvark"
  expect_error(search_names("Aardvark", "name"))
})

test_that("invalid column choice returns an error", {
  # search for "Avon in column "Aardvark"
  expect_error(search_names("Avon", "Aardvark"))
})

test_that("incomplete arguments returns an error", {
  # search for "Avon in column "Aardvark"
  expect_error(search_names("Aardvark"))
})