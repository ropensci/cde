library(cde)
context("search_sites")

test_that("dimensions ofsearch_site dataframe are as expected", {
  # search for sites containing "Avon" in their name
  test_search_avon<-search_sites("Avon", "name")
  # check that the dimensions are 516 rows, 17 cols
  expect_that(all(dim(test_search_avon)== c(103, 6), equals(TRUE)))
  
})

