#' Assembles FAO's Global Fishery and Aquaculture Production Data from scratch
#'
#' \code{rebuild_fish} takes all CSV files from an unzipped folder of FAO data and merges them into "tidy" (i.e., long) format

#' @param path_to_folder Path to unzipped (extracted) folder of FAO data
#' @return A merged, tidy dataset.
#' @examples
#' clean_fish("~/OneDrive - american.edu/GlobalProduction_2019.1.0")
#' clean_fish("C:\\Users\\kdgor\\OneDrive - american.edu\\GlobalProduction_2019.1.0")

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

  # generate path to cl files in batch:
  code_files<-fish_files[grep("CL", fish_files)]
  path_to_cl<-unlist(map(path_to_folder, ~paste(.x, code_files, sep="/")))

  time_series_join<-time_series

  for (i in 1:nrow(ds)){
    # TRUE/FALSE: is there a filename listed in Codelist_id?
    if(!is.na(ds$Codelist_id[i])){
      # Use ds file to generate path_to_cl individually
      code_file_i<-paste(ds$Codelist_id[i], ".csv", sep = "")
      path_to_cl<-paste(path_to_folder, code_file_i, sep = "/")
      cl_i<-read.csv(path_to_cl, check.names = FALSE) # check.names = FALSE to prevent R from adding "X" in front of column "3Alpha_Code" - creates problems because this is the matching column for merging with time series

      # Many CL files have "Name" as a column, also Name_En, Name_Fr, Name_Es, etc
      # To disambiguate, append "Concept_ID" from DS file to all columns in CL that begin with "Name"
      concept_names<-paste(ds$Concept_id[i], names(cl_i)[grep("Name", names(cl_i))], sep="_")
      names(cl_i)[grep("Name", names(cl_i))]<-concept_names


      names(cl_i)<-tolower(names(cl_i)) # convert all cl headers to lowercase
      merge_col<-tolower(ds$Codelist_Code_id[i]) # do the same to DS file's code ID so it matches with cl

      # Note: the following code does not work: #time_series_join<-left_join(time_series, cl_i, by = c(names(time_series)[i] = merge_col))
      # the argument "by" needs to take on the form of join_cols as shown below
      firstname<-names(time_series)[i]
      join_cols<-merge_col
      names(join_cols)<-firstname
      time_series_join<-left_join(time_series_join, cl_i, by = join_cols)
    }
    # Expected warning: Coerces from factor to character because time_series$SPECIES (nlevels=2341) and CL_FI_SPECIES_GROUPS.csv column "3alpha_code" (nlevels = 12751) have different number of factor levels
    # Expected warning: Coerces from factor to chracter because time_series$UNIT and CL_FILE_UNIT.csv column "code" have different number of factor levels
    # Expected warning: Coerces from factor to character because time_series$SYMBOL and CL_FI_SYMBOL.csv column "symbol" have diff number of factors
  }

  time_series_join

}
