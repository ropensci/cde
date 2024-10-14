library(cde)
context("testing plotting and printing")

test_that("plotting WBID data results in error", {
  # retrieve data for column "Aardvark"
  testframe<-get_rnag("GB112071065700", "WBID")
  expect_error(plot(testframe))
})

 test_that("invalid scheme returns an error", {
   # specify scheme as "Aardvark"
   expect_error(plot(get_status("Avon Warwickshire", "MC"),scheme="Aardvark"))
 })
 
 test_that("plotting status returns a null", {
   # download all Avon Warwickshire MC chemical status
   testframe<-get_status("Avon Warwickshire", "MC", level="Chemical")
   # check that the plot output is null
   expect_true(is.null(plot(testframe)))
 })

 test_that("plotting status (one year) returns a null", {
    # download Avon Warwickshire MC chemical status for 2015
    testframe<-get_status("Avon Warwickshire", "MC", startyr=2015, level="Chemical")
    # check that the plot output is null
    expect_true(is.null(plot(testframe)))
 })

 test_that("plotting objectives (one year/class) returns a null", {
    # download objectives for Avon Warwickshire MC chemical status in 2027
    testframe<-get_objectives("Avon Warwickshire", "MC", year=2027, level="Chemical")
    # check that the plot output is null
    # expect_true(is.null(plot(testframe)))
    expect_error(plot(testframe))
 })
 
 test_that("plotting objectives returns a null", {
   # download objectives for Avon Warwickshire MC chemical status
   testframe<-get_objectives("Avon Warwickshire", "MC", level="Chemical")
   # check that the plot output is null
   expect_true(is.null(plot(testframe)))
 })
 
 test_that("plotting rnag returns a null", {
   # download Avon Warwickshire MC RNAG
   testframe<-get_rnag("Avon Warwickshire", "MC")
   # check that the plot output is null
   expect_true(is.null(plot(testframe)))
 })
 
 test_that("plotting pa returns a null", {
   # download all pa in Thames RBD
   testframe<-get_pa("Thames", "RBD")
   # check that the plot output is null
   expect_true(is.null(plot(testframe)))
 })
 
 test_that("printing returns a null", {
   # download all pa in Thames RBD and print
   testframe<-get_pa("Thames", "RBD")
   expect_true(is.null(print(testframe)))
 })
 
 test_that("printing (smaller) returns a null", {
   # download smaller pa list and print
   testframe<-print(get_pa(ea_name="GB105033042920", column="WBID"))
   expect_true(is.null(testframe))
 })
 