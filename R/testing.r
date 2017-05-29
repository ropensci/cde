# testing code for different functions

# testing search_sites function

test1<-search_sites("water.body", "Avon")
# should return a df with 103 obs of 7 variables (ea sites only)

test2<-search_sites("aardvark", "Avon")
# should return NULL, with message

# testing ea data extraction function
library(curl)
library(data.table)

test3 <- download_ea("GB520804714300", "Waterbody.ID")
# test3 should be df with 309 obs of 18 variables
test4 <- download_ea("GB520804714300", "aardvark")
# test4 returns NULL, with message
test5 <- download_ea("xxx1", "Waterbody.ID")
# test5 should be NULL with message
# whole RBD
test6 <- download_ea("Thames" , "River.basin.district")
# test6 should return df with 99292 rows of 18 vars


# test lapply version of extraction function
test_vector<-test1$Waterbody.ID[1:20]

test_lapply<-lapply(test_vector, extract_ea_data, "Waterbody.ID")