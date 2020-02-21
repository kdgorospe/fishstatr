rm(list=ls())
path_to_folder<-"C:\\Users\\kdgor\\OneDrive - american.edu\\GlobalProduction_2019.1.0"

rebuild_fish<-function(path_to_folder){
  require(dplyr)
  require(purrr)
  require(readxl) # part of tidyverse but still need to load readxl explicitly, because it is not a core tidyverse package

  # list files
  fish_files<-list.files(path_to_folder)

  # read .xlsx file (explains data structure of time series)
  # IMPORTANT: column ORDER (ABCDEF) in DS file should match columns ABCDEF in time series for looping to work below
  # each row gives info for how this time series column should be merged with a code list (CL) file
  ds_file<-fish_files[grep("DS", fish_files)]
  path_to_ds<-paste(path_to_folder, ds_file, sep = "/")
  ds<-read_excel(path_to_ds)
  # remove metadata rows
  data_start<-grep("Order", ds[,1]) # find true header row based on column name "Order"
  colnames(ds)<-as.character(unlist(ds[data_start,]))
  ds<-ds[-1,]

  # remove non CSVs (do this to ignore "CL_History.txt" file)
  fish_files<-fish_files[grep(".csv", fish_files)]

  # read in time series.csv
  time_files<-fish_files[grep("TS", fish_files)]
  path_to_ts<-paste(path_to_folder, time_files, sep = "/")
  time_series<-read.csv(path_to_ts)

  # read in code list files
  #code_files<-fish_files[grep("CL", fish_files)]
  #path_to_cl<-unlist(map(path_to_folder, ~paste(.x, code_files, sep="/")))
  # Use ds file to geerate path_to_cl
  for i in (1:nrow(ds)){
    # TRUE/FALSE: is there a filename listed in Codelist_id?
    if(!is.na(ds$Codelist_id[i])){
      code_file_i<-paste(ds$Codelist_id[i], ".csv", sep = "")
      path_to_cl<-paste(path_to_folder, code_file_i, sep = "/")
      cl_i<-read.csv(path_to_cl)
      names(cl_i)<-tolower(names(cl_i)) # convert all cl headers to lowercase
      merge_col<-tolower(ds$Codelist_Code_id[i]) # do the same to DS file's code ID so it matches with cl

      # Note: the following code does not work: #time_series_join<-left_join(time_series, cl_i, by = c(names(time_series)[i] = merge_col))
      # the argument "by" needs to take on the form of join_cols as shown below
      firstname<-names(time_series)[1]
      join_cols<-merge_col
      names(join_cols)<-firstname
      time_series_join<-left_join(time_series, cl_i, by = join_cols)





    }
  }

  cl_1<-read.csv((path_to_cl)[1]) # Need to make this flexible to length of path_to_cl
  cl_2<-read.csv((path_to_cl)[2])
  cl_3<-read.csv((path_to_cl)[3])
  cl_4<-read.csv((path_to_cl)[4])
  cl_5<-read.csv((path_to_cl)[5])
  cl_6<-read.csv((path_to_cl)[6])

  # Combine Time Series with Code Files
  left_join(time_series, cl_1)


}
