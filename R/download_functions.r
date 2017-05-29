# download functions for different agency sources

#  packages required for all functions
library(readxl)
library(curl)

# need to load system files before extraction
# currently ea_wbids and sepa_links
load('ea_wbids.rda')
load('sepa_links.rda')


# keep subsetting functions separate from download as far as possible to 
# reduce code redundency. Pass type, column etc from main function

# function for downloading from SEPA site

download_sepa<-function(url_string){
  curl_download(url_string, 'temp.xls', mode="wb")
  sepa_data<-read_excel('temp.xls')
  # need to derive year info to allow correct extraction
  # delete the intermediate xl file
  unlink('temp.xls')

  # need to rewrite the column names (will depend on the year/type)
  old_names<-names(sepa_data)
  better_names<-make.names(old_names)
  names(sepa_data)<-better_names
  #  str(sepa1)
  return(sepa_data)
  
  # end of function
}

################################

# EA data download - currently does some subsetting as well which needs to 
# be taken out to main function, but have to pass number in
# col_value and col_name should be in main really

download_ea<-function(col_value=NULL, col_name=NULL){
  # what is this going to allow to be parsed in columns?
  # wbid, catchment, RBD, type?
  # add in year selection too
  if (!is.null(col_name) & !is.null(col_value)){
    # set up a smaller vector that just contains the names of columns
    # that can be exracted by
    if (tolower(col_name) %in% tolower(names(ea_wbids))){
      # match column to  value in ea_wbids
      #col_name<-"Waterbody.ID"
      #col_value<-"xxx1"
      catch_num<-ea_wbids$Number[which(ea_wbids[,col_name]==col_value)]
      if (length(catch_num)<1){
        message("Column value not found")
      }else{
        temp<-tempfile()
        downloadurl<-paste0("http://environment.data.gov.uk/catchment-planning/RiverBasinDistrict/", catch_num, "/classification?item=all&status=all&format=csv")
        downloadurl
        curl_download(downloadurl, temp, mode="wb")
        # extract data from zipfile to df
        catchment_data<-fread(unzip(temp, junkpaths=TRUE), stringsAsFactors = FALSE, check.names=TRUE, data.table=FALSE)
        unlink(temp)
        # subset catchment_data based on vector of row numbers
        data_subset<-catchment_data[which(catchment_data[,col_name]==col_value), ]
      }
      # repeat for other levels
      # need to review how to treat data from different cycles - 2015 only has 2
      # 2013-14 has both
    }else{
      message("Column specified should be one of the columns of the allowed")
    }
  }
  # end of function
}