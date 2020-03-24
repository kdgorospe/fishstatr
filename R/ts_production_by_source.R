tidy_fish=tmp_fish
year_start=2012
year_end=NA
fish_var="quantity"
fish_level="scientific_species_name"
fish_name="all"
fish_unit="t"
geo_level="country"
geo_name="all"
output_path="~/"
combine_aquaculture=TRUE
incl_CHN=TRUE



ts_production_by_source <- function(tidy_fish,
                          year_start,
                          year_end=NA,
                          fish_var="quantity",
                          fish_level="total",
                          fish_name=NA,
                          fish_unit="t",
                          country,
                          output_path="~/",
                          combine_aquaculture=TRUE,
                          incl_CHN=TRUE) {

  # UNIVERSAL FILTERING and CLEANING: MOVE THIS INTO ANOTHER FUNCTION?
  # Deal with countries that have no iso3 alpha codes:
  # For Sudan, country column (aka iso numeric code) changed from 736 to 729 after year 2011
  # Fill in iso3 alpha = SUD for iso3 numeric 736
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
      mutate(source_name_en = as.factor(source_name_en))
  }

  # Match column name to geo_level parameter
  # In tmp_fish there are more levels in iso3 than in "country"; for now, use iso3 as the reporting level
  # other notes: un_a3 in worldmap == "country" in tmp_fish == code in FishStatJ (3 digit UNM49 country ID number)
  if (geo_level == "country" | country == "all"){
    geography <- "country_iso3_code"
  } else if (geo_level == "continent"){
    geography<-"continent_group"
  } else if (geo_level == "region"){
    geography<-"georegion_group"
  }

  # AGGREGATE:
  if(fish_name=="all" & geo_name=="all"){
    year_geo_taxa_fish <- dat_fish %>%
      group_by(year, get(geography), species_scientific_name, source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
      droplevels()
  }

  # histogram?

  # if range of years given create:
  # LINE GRAPH - each line represents country x species combo tracing percent aquaculture vs capture through time
  # DENSITY DISTRIBUTION THROUGH TIME - can use this to estimate a cutoff point?

  # if only one year given create:
  # SCATTER PLOTS - each dot represents country x species combo of percent aquaculture vs capture through time
  # Output table of values (using same year of current analysis - 2012) to merge with S_net

}
