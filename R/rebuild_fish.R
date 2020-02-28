#' Assembles FAO's Global Fishery and Aquaculture Production Data from scratch
#'
#' \code{rebuild_fish} Extracts data from FAO zipped file and merges them into "tidy" (i.e., long) format

#' @param path_to_zipfile Name of zipped file of FAO data (if in current directory); otherwise, specify path to file
#' @return A merged, tidy dataset.
#' @examples
#' rebuild_fish("~/OneDrive - american.edu/FAO Data/Production-Global/ZippedFiles/GlobalProduction_2019.1.0.zip")
#' rebuild_fish("C:\\Users\\kdgor\\OneDrive - american.edu\\FAO Data\\Production-Global\\ZippedFiles\\GlobalProduction_2019.1.0.zip")
rebuild_fish <- function(path_to_zipfile) {
  require(tools) # needed for file_path_sans_ext
  require(dplyr)
  require(purrr)
  require(readxl) # part of tidyverse but still need to load readxl explicitly, because it is not a core tidyverse package

  # The following ensures unzipped folder is created in the same directory as the zip file (can be different from the working directory)
  # set outdir
  if (file.exists(basename(path_to_zipfile))) { # if file is in current directory and only file name was given
    outdir <- getwd()
  } else if (file.exists(path_to_zipfile)) { # if file path was given
    outdir <- dirname(path_to_zipfile)
  } else {
    stop("Check path_to_zipfile")
  }

  foldername <- file_path_sans_ext(basename(path_to_zipfile))
  outfolder <- paste(outdir, foldername, sep = "/")
  unzip(path_to_zipfile, exdir = outfolder) # Problem: if unable to unzip folder, still creates outfolder how to supress this?
  setwd(outfolder)
  # list files
  fish_files <- list.files(outfolder)

  # read .xlsx file (explains data structure of time series)
  # IMPORTANT: column ORDER (ABCDEF) in DS file should match columns ABCDEF in time series for looping to work below
  # each row gives info for how this time series column should be merged with a code list (CL) file
  ds_file <- fish_files[grep("DSD", fish_files)]
  path_to_ds <- paste(outfolder, ds_file, sep = "/")

  # skip removes title row
  ds <- read_excel(path_to_ds, skip=1)

  # manually correct ds file's codelist ID column:
  ds <- ds %>%
    mutate(Codelist_Code_id = case_when(
      Concept_id == "SOURCE" ~ "IDENTIFIER",
      Concept_id == "SYMBOL" ~ "SYMBOL",
      Concept_id != "SYMBOL|SOURCE" ~ Codelist_Code_id
    ))


  # remove non CSVs (do this to ignore "CL_History.txt" file)
  fish_files <- fish_files[grep(".csv", fish_files)]

  # read in time series.csv
  time_files <- fish_files[grep("TS", fish_files)]
  path_to_ts <- paste(outfolder, time_files, sep = "/")
  time_series <- read.csv(path_to_ts)

  # generate path to cl files in batch:
  # DELETE?
  code_files <- fish_files[grep("CL", fish_files)]
  path_to_cl <- unlist(map(outfolder, ~ paste(.x, code_files, sep = "/")))

  time_series_join <- time_series

  for (i in 1:nrow(ds)) {
    # TRUE/FALSE: is there a filename listed in Codelist_id?
    if (!is.na(ds$Codelist_id[i])) {
      # Use ds file to generate path_to_cl individually
      code_file_i <- paste(ds$Codelist_id[i], ".csv", sep = "")
      path_to_cl <- paste(outfolder, code_file_i, sep = "/")
      cl_i <- read.csv(path_to_cl, check.names = FALSE) # check.names = FALSE to prevent R from adding "X" in front of column "3Alpha_Code" - creates problems because this is the matching column for merging with time series

      # Many CL files have "Name" as a column, also Name_En, Name_Fr, Name_Es, etc
      # Also, "Identifier", "Major Group", and "Code" are common across some CL files
      # To disambiguate, append "Concept_ID" from DS file to all columns in CL that contain these terms
      # use word boundaries ("\\b") to match Code exactly, and leave UN_code alone (which is needed for merging later)
      concept_names <- paste(ds$Concept_id[i], names(cl_i)[grep("Name|Major Group|Identifier|\\bCode\\b", names(cl_i))], sep = "_")
      names(cl_i)[grep("Name|Major Group|Identifier|Code", names(cl_i))] <- concept_names


      names(cl_i) <- tolower(names(cl_i)) # convert all cl headers to lowercase
      merge_col <- tolower(ds$Codelist_Code_id[i]) # do the same to DS file's code ID so it matches with cl


      # If factor...
      if (is.factor(cl_i[[merge_col]])) {
        # ...Test if factor levels need to be merged?
        if (!nlevels(cl_i[[merge_col]]) == nlevels(time_series_join[[names(time_series_join)[i]]])) {
          # combined <- sort(union(time_series_join[[names(time_series_join)[i]]], levels(cl_i[[merge_col]])))
          levels(time_series_join[[names(time_series_join)[i]]]) <- levels(cl_i[[merge_col]])
        }
      }
      # This avoids warnings about unequal factor levels below

      # Can't just merge by column number:
      # In Time Series, column COUNTRY, AREA, SOURCE, SPECIES, and UNIT correspond to column 1 in their respective CL files
      # but in Time Series, column SYMBOL corresponds to column 2

      # Note: the following code does not work: #time_series_join<-left_join(time_series, cl_i, by = c(names(time_series)[i] = merge_col))
      # the argument "by" needs to take on the form of join_cols as shown below
      firstname <- names(time_series_join)[i]
      join_cols <- merge_col
      names(join_cols) <- firstname


      time_series_join <- left_join(time_series_join, cl_i, by = join_cols)
    }
    # Expected warning: Coerces from factor to character because time_series$SPECIES (nlevels=2341) and CL_FI_SPECIES_GROUPS.csv column "3alpha_code" (nlevels = 12751) have different number of factor levels
    # Expected warning: Coerces from factor to chracter because time_series$UNIT and CL_FILE_UNIT.csv column "code" have different number of factor levels
    # Expected warning: Coerces from factor to character because time_series$SYMBOL and CL_FI_SYMBOL.csv column "symbol" have diff number of factors
  }

  time_series_join
}
