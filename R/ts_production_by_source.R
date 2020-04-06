# PRODUCE bar plot through time of wild vs aquaculture capture for a specific country/geo_level and specific species/fish_level
# See Figure 7 from SOFIA 2018 report

tidy_fish=tmp_fish
year_start=2012
year_end=2015
#year_start=2000
#year_end=2005
fish_var="quantity"
fish_level="species_scientific_name" # can also be set to total, sum of all seafood production (i.e., no taxa aggregation)
fish_name="all" # all means produce all aggregation units (e.g., all species), rather than specific one
# FIX IT - make sure that if fish_level==total and fish_name is also specified, that this doesn't break code
fish_unit="t"
geo_level="country" # can also be set to "all" - i.e., global production (i.e., no geographic aggregation)
geo_name="USA"
output_path="~/"
combine_aquaculture=TRUE
incl_CHN=TRUE
percent_aqua=TRUE # FIX IT - make this an option where if FALSE, it calculate percent wild capture



ts_production_by_source <- function(tidy_fish,
                          year_start,
                          year_end=NA,
                          fish_var="quantity",
                          fish_level="total",
                          fish_name=NA,
                          fish_unit="t",
                          country,
                          output_path=NA,
                          combine_aquaculture=TRUE,
                          incl_CHN=TRUE) {

  require(dplyr)
  require(ggplot2)
  require(stringr)
  # UNIVERSAL FILTERING and CLEANING: MOVE THIS INTO ANOTHER FUNCTION?
  # Deal with countries that have no iso3 alpha codes:
  # For Sudan, country column (aka iso numeric code) changed from 736 to 729 after year 2011
  # Fill in iso3 alpha = SUD for iso3 numeric 736
  # FIX IT - this should be saved as a function in a separate .R file
  dat_fish <- tidy_fish %>%
    mutate(country_iso3_code = replace(country_iso3_code, country==736, "SDN")) %>%
    # For Channel Islands, drop for now: some of the channel islands (Jersey, Isle of Man, Guernsey) have their own alpha codes but they're all lumped together in FAO data as iso_n3=830
    filter(country!=830) %>%
    # Drop iso_n3=896 (used for "Areas not elsewhere specified", aka country ID not known?)
    filter(country!=896) %>%
    # Filter for desired units
    filter(unit == fish_unit) %>%
    # NOTE: in some cases, countries explicitly report "zero" for certain groups of fish and/or sources, while others are NA (likely also zero?)
    # i.e., FIX IT: Ask JG/FAO, is there a difference between countries that explicitly report zeroes vs. NAs?
    # For now, remove all entries where quantity=0 (otherwise, countries are colored in as 0 as opposed to being NA)
    filter(get(fish_var) != 0)


  # Filter years
  if (is.na(year_end)){
    all_years <- year_start
  } else {
    all_years <- seq(from = year_start, to = year_end, by = 1)
  }
  dat_fish <- dat_fish %>%
    filter(year %in% all_years)

  # If desired, combine aquaculture sources
  if (combine_aquaculture==TRUE) {
    dat_fish <- dat_fish %>%
      mutate(source_name_en = as.character(source_name_en)) %>%
      mutate(source_name_en = replace(source_name_en, str_detect(source_name_en, "Aquaculture"), "Aquaculture production")) %>%
      mutate(source_name_en = as.factor(source_name_en)) %>%
      droplevels()
  }

  # Match column name to geo_level parameter
  # In tmp_fish there are more levels in iso3 than in "country"; for now, use iso3 as the reporting level
  # other notes: un_a3 in worldmap == "country" in tmp_fish == code in FishStatJ (3 digit UNM49 country ID number)
  if (geo_level == "country" | geo_name == "all"){
    geography <- "country_iso3_code"
  } else if (geo_level == "continent"){
    geography<-"continent_group"
  } else if (geo_level == "region"){
    geography<-"georegion_group"
  }

  # AGGREGATE:
  if (fish_level!="total" & geo_level!="total"){ # if specific fish_level (e.g., species) and geo_level (e.g., country) is desired
    year_geo_taxa_fish <- dat_fish %>%
      group_by(year, get(geography), get(fish_level), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
      rename(!!fish_level := 'get(fish_level)') %>%
      droplevels()
  } else if (fish_level=="total" & geo_level!="total"){ # if desired output is total seafood, but specific country, remove fish_level from aggregation
    year_geo_taxa_fish <- dat_fish %>%
      group_by(year, get(geography), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>%
      rename(!!fish_level := 'get(fish_level)') %>%
      droplevels()
  } else if (fish_level!="total" & geo_level=="total"){ # if desired output is global production, but specific fish species, remove geo_level from aggregation
    year_geo_taxa_fish <- dat_fish %>%
      group_by(year, get(fish_level), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>%
      rename(!!fish_level := 'get(fish_level)') %>%
      droplevels()
  } else if (fish_level=="total" & geo_level=="total"){ # if desired output is global production across all species (i.e., reproduce SOFIA figure)
    year_geo_taxa_fish <- dat_fish %>%
      group_by(year, source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>%
      rename(!!fish_level := 'get(fish_level)') %>%
      droplevels()
  }


  # if specificed, filter for specific fish_name or geo_name
  # FIX IT careful, if these are not specified, function will produce every aggregation unit in specified fish_level or geo_level (i.e., could potentially produce every species for every country!)
  if (fish_name!="all"){
    year_geo_taxa_fish <- year_geo_taxa_fish %>%
      filter(fish_level==fish_name)
  } else if (geo_name!="all"){
    year_geo_taxa_fish <- year_geo_taxa_fish %>%
      filter(!!rlang::sym(geography)==geo_name) # TIDY EVALUATION
  }



  # NOTE: right_join(by = get(geography)) does not work, need to supply a named column
  first_geo <- geography
  join_geo <- geography
  names(join_geo) <- first_geo

  first_fish <- fish_level
  join_fish <- fish_level
  names(join_fish) <- fish_level

  # if desired, calculate proportions
  if(percent_aqua==TRUE){
    # year_geo_taxa_totals sums production (aquaculture + capture) within year, country, taxa
    year_geo_taxa_totals <- year_geo_taxa_fish %>%
      group_by(year, get(geography), get(fish_level)) %>%
      #group_by(year, get(geography)) %>%
      #group_by(year) %>%
      summarize(fish_all_sources = sum(fish_sum)) %>%
      rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
      rename(!!fish_level := 'get(fish_level)')

    # year_geo_taxa_aqua only keeps aquaculture production, drops all capture
    year_geo_taxa_aqua<-year_geo_taxa_fish %>%
      filter(source_name_en!="Capture production")

    # year_geo_taxa_prop: match fish_sum column of just aqua data (i.e., total amt of aqua) back to totals data
    # rows with no matching aqua are filled in as NAs and replaced with zeroes - i.e., years where only capture was produced
    year_geo_taxa_prop <- year_geo_taxa_totals %>%
      left_join(year_geo_taxa_aqua, by=c("year", join_geo, join_fish)) %>%
      rename(fish_aqua = fish_sum) %>%
      mutate(fish_aqua = replace(fish_aqua, is.na(fish_aqua), 0)) %>%
      mutate(fish_plot = fish_aqua / fish_all_sources) %>%
      arrange(desc(fish_plot, get(geography), get(fish_level))) %>%
      select(-source_name_en)

    plot_dat <- year_geo_taxa_prop
  }


  # Plot Time series
  # FIX IT - produce warning message for when all_years is less than 2
  if (all_years > 1) {
    ts <- ggplot(plot_dat)
### CONTINUE HERE


      ts_file <- paste("plot_sd_cutoff_", sd_cutoff, "_proportion_from_capture_time_series.png", sep = "")

      ts + ggsave(filename = ts_file, width = 5.5, height = 4.25, units = "in")
    }

  }







  # histogram?

  # if range of years given create:
  # LINE GRAPH - each line represents country x species combo tracing percent aquaculture vs capture through time
  # DENSITY DISTRIBUTION THROUGH TIME - can use this to estimate a cutoff point?

  # if only one year given create:
  # SCATTER PLOTS - each dot represents country x species combo of percent aquaculture vs capture through time
  # Output table of values (using same year of current analysis - 2012) to merge with S_net

}
