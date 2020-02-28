#' Cleans FAO's Fishery and Aquaculture Production Data
#'
#' \code{clean_fish} takes FAO's Global Fishery and Aquaculture Production data downloaded from FishStatJ, cleans the time series data and makes it "tidy" (i.e., long format).

#' @param path_to_file File path to FAO data
#' @param dataset FAO dataset
#' @return A cleaned, tidy dataset.
#' @examples
#' clean_fish("~/OneDrive - american.edu/FAO Global capture production-QUANTITY.csv")
#' clean_fish("C:\\Users\\kdgor\\OneDrive - american.edu\\FAO Global capture production-QUANTITY.csv")

clean_fish<-function(path_to_file, dataset="Catch"){
  check_pkg_deps() # packages are automatically loaded, but will show error message if one of the needs to be installed
  fish_file<-read.csv(path_to_file)
  #Remove last three rows: two rows of summed tonnage and abundance + citation
  fish_file<-fish_file %>%
    dplyr::filter(!str_detect((fish_file)[,1], pattern = "Total")) %>%
    dplyr::filter(!str_detect(.[,1], pattern = "www"))

  #Year column issues: "...", "-", "0 0", and blanks
  #Find year columns (i.e., start_with X) and mutate those columns by replacing "..." with NAs
  fish_clean_years<-fish_file %>%
    mutate_at(vars(starts_with("X")), ~na_if(., "...")) %>%
    #Same, but now replace "-" with NAs
    mutate_at(vars(starts_with("X")), ~na_if(., "-")) %>%
    #Same, but now replace "0 0" with NAs
    mutate_at(vars(starts_with("X")), ~na_if(., "0 0")) %>%
    #Same, but now remove "F"s
    mutate_at(vars(starts_with("X")), ~str_replace_all(., "F", "")) %>%
    #Same, but not trim white space
    mutate_at(vars(starts_with("X")), ~str_trim(.)) %>%
    #Same, but now convert to numeric
    mutate_at(vars(starts_with("X")), ~as.numeric(.))

  tmp_fish<-fish_clean_years %>%
    pivot_longer(cols = starts_with("X"),
                 names_to = "Year",
                 names_prefix = "X",
                 names_ptypes = list(Year=integer()),
                 values_to = dataset,
                 values_drop_na = TRUE) #this last option drops years with NAs
}
