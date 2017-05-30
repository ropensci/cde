# testing code for different functions

# testing search_sites function

#library(curl)
#library(data.table)

#test1<-search_sites("Avon", "name")
# should return a df with 103 obs of 9 variables (ea sites only)

#test2<-search_sites("Avon", "aardvark")
# should return NULL, with message

# testing ea data extraction function

#test3 <- download_ea("GB520804714300", "wbid")
# test3 should be df with 309 obs of 24 variables
#test4 <- download_ea("GB520804714300", "aardvark")
# test4 returns NULL, with message
#test5 <- download_ea("xxx1", "wbid")
# test5 should be NULL with message
# whole RBD
#test6 <- download_ea("Thames" , "RBD")
# test6 should return df with 99292 rows of 18 vars


# test lapply version of extraction function
#test_vector<-test1$Waterbody.ID[1:20]

#test_lapply<-lapply(test_vector, download_ea, "Waterbody.ID")


#test<-download_ea("GB106039017390", "wbid")

#test<-download_ea("Chelmer", "oc")

#test<-download_ea("Combined Essex", "mc")


#test2<-search_sites("Avon", "waterbody")